;; Copyright (C) 2024-2025 Skylar Astaroth <cobra@vern.cc>
;; Copyright (C) 2026 darlopvil (modified for unfunny-fork)
;;
;; This file is part of UNfunny
;;
;; UNfunny is free software: you can redistribute it and/or modify it under the
;; terms of the GNU Affero General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of  MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License
;; for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (unfunny pages)
  #:use-module (unfunny scraping)
  #:use-module (unfunny templates)
  #:use-module (unfunny upstream)
  #:use-module (unfunny cache)
  #:use-module (unfunny session)
  #:use-module (htmlprag)
  #:use-module (web response)
  #:use-module (web http)
  #:use-module (web uri)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:export (error-page
            home-page
            channel-page
            featured-page
            tag-page
            user-page
            channel-next-page
            post-page
            proxy-page))

(define good-response
  (build-response #:code 200
                  #:headers `((content-type . (text/html)))))

(define (redirect response code)
  (values (build-response #:code code
                          #:headers `((location . ,(parse-header
                                                     'location
                                                     (uri-path
                                                       (response-location
                                                         response))))))
          "Redirect"))

(define (error-page code)
  (values (build-response
            #:code code
            #:headers `((content-type . (text/html))))
          (shtml->html (error-template code))))

;; Raw (undecoded) value of KEY in a query string, or #f.  Tolerates a
;; missing query, empty parts and parameters without '='.
(define (query-ref query key)
  (let ((prefix (string-append key "=")))
    (and query
         (let loop ((parts (string-split query #\&)))
           (cond ((null? parts) #f)
                 ((string-prefix? prefix (car parts))
                  (substring (car parts) (string-length prefix)))
                 (else (loop (cdr parts))))))))

;; What to answer when iFunny answers CODE instead of the page or file.
(define (upstream-status code)
  (cond ((memv code '(404 410)) 404)
        ((memv code '(403 429)) 503)
        (else 502)))

;; Runs THUNK, which talks to iFunny and builds a response.  Any failure,
;; including a page that cannot be read, becomes an error page (504 on
;; timeout, 503 when iFunny's API refuses the instance, 502 otherwise)
;; instead of an uncaught exception, which Guile answers with a blank 500.
(define (with-upstream-errors thunk)
  (catch #t
    thunk
    (lambda (key . args)
      (display (string-append
                 ";\tUpstream: " (symbol->string key)
                 (if (pair? args)
                   (string-append " (" (object->string (car args)) ")")
                   "")))
      (error-page (case key
                    ((upstream-timeout) 504)
                    ((api-error) 503)
                    (else 502))))))

;; How long a rendered page stays cached, in seconds.  A post hardly ever
;; changes (only its counters do); feeds move faster.  When Featured cannot be
;; loaded, the home page says so for a minute before asking iFunny again.
(define post-ttl 3600)
(define feed-ttl 600)
(define home-failure-ttl 60)

(define (render shtml)
  (string-replace-substring (shtml->html shtml) "&amp;" "&"))

;; The cached page under KEY, or else the result of THUNK, which renders it
;; (and caches it if it wants to).  Concurrent misses share one THUNK call.
(define (cached-page key thunk)
  (let ((cached (cache-ref key)))
    (if cached
      (begin
        (display ";\tCache: hit")
        (values good-response cached))
      (shared-flight key thunk (lambda () (error-page 502))))))

;; Pages are cached by path: the same path always maps to the same iFunny
;; page.  Only successful pages are stored, so a failure is retried on the
;; next visit instead of sticking.
(define (generic-page procedure path ttl)
  (cached-page path (lambda () (generic-page* procedure path ttl))))

(define (generic-page* procedure path ttl)
  (with-upstream-errors
    (lambda ()
      (let ((resp "")
            (body "")
            (url (if (string-prefix? "/br/" path)
                   (string-append "https://br.ifunny.co" (string-drop path 3))
                   (string-append "https://ifunny.co" path))))
        (receive (_resp _body)
                 (fetch url)
                 (set! resp _resp)
                 (set! body _body))
        (remember-cookies! resp)
        (display (string-append ";\tStatus: "
                                (number->string (response-code resp))))
        (cond ((equal? (response-code resp) 200)
               (let* ((state (initial-state body))
                      (html (begin
                              (note-channels! state)
                              (render (procedure state)))))
                 (cache-put! path html ttl)
                 (values good-response html)))
              ((and (>= (response-code resp) 300)
                    (<= (response-code resp) 399))
               (redirect resp (response-code resp)))
              (else
               (error-page (upstream-status (response-code resp)))))))))

(define (post-page path)
  (display ";\tHandler: post-page")
  (generic-page post-template path post-ttl))

(define (channel-page path)
  (display ";\tHandler: channel-page")
  (generic-page feed-template path feed-ttl))

(define (featured-page path)
  (display ";\tHandler: featured-page")
  (generic-page feed-template path feed-ttl))

(define (tag-page path)
  (display ";\tHandler: tag-page")
  (generic-page feed-template path feed-ttl))

;; iFunny's channel list, as last seen in one of its pages.  Further pages of
;; channels come from the API, whose answers do not include it.
(define known-channels #())

(define (note-channels! state)
  (let ((channels (ref state "channels")))
    (when (and (vector? channels) (> (vector-length channels) 0))
      (set! known-channels channels))))

;; Fetches one of iFunny's HTML pages only for the session it sets.
(define (renew-session path)
  (lambda ()
    (receive (response body) (fetch (string-append "https://ifunny.co" path))
      (display (string-append ";\tSession: "
                              (number->string (response-code response))))
      (remember-cookies! response))))

;; STATE with its feed's items and pagination replaced.
(define (with-feed state items pagination)
  (acons "feed"
         (acons "items" (list->vector items)
                (acons "pagination" pagination
                       (remove (lambda (entry)
                                 (member (car entry) '("items" "pagination")))
                               (or (ref state "feed") '()))))
         (remove (lambda (entry) (equal? (car entry) "feed")) state)))

;; A profile's pages hold very few posts (often 1 to 3, it is iFunny's page
;; size).  The rest of the timeline comes from the API, so each UNfunny page
;; gathers up to timeline-target posts with at most timeline-calls more
;; requests.  Any failure just stops gathering: what is there is shown.
(define timeline-target 10)
(define timeline-calls 3)

(define (gather-timeline state)
  (let ((nick (ref state "user" "data" "nick")))
    (let loop ((items (let ((items (ref state "feed" "items")))
                        (if (vector? items) (vector->list items) '())))
               (pagination (ref state "feed" "pagination"))
               (calls 0))
      (let ((next (ref pagination "next")))
        (if (and (< (length items) timeline-target)
                 (< calls timeline-calls)
                 (non-empty-string? nick)
                 (eq? (ref pagination "hasNext") #t)
                 (non-empty-string? next)
                 (api-available?))
          (let ((data (catch #t
                        (lambda ()
                          (api-get (string-append "/v1/user/" (uri-encode nick)
                                                  "/timeline/" next)
                                   (renew-session
                                     (string-append "/user/" (uri-encode nick)))))
                        (lambda _ #f))))
            (if (and data (vector? (ref data "items")))
              (loop (append items (vector->list (ref data "items")))
                    (ref data "pagination")
                    (1+ calls))
              (with-feed state items pagination)))
          (with-feed state items pagination))))))

(define (user-page path)
  (display ";\tHandler: user-page")
  (generic-page (lambda (state) (user-template (gather-timeline state)))
                path feed-ttl))

;; Further pages of a channel, from the API (its HTML routes do not take the
;; cursors channels use).  CURSOR was checked by the caller.
(define (channel-next-page path channel cursor)
  (display ";\tHandler: channel-next-page")
  (cached-page
    path
    (lambda ()
      (with-upstream-errors
        (lambda ()
          (let* ((data (api-get (string-append "/v1/feeds/channels/"
                                               (uri-encode channel)
                                               "?next=" cursor)
                                (renew-session
                                  (string-append "/channels/"
                                                 (uri-encode channel)))))
                 (name (or (any (lambda (known)
                                  (and (equal? (ref known "path") channel)
                                       (ref known "name")))
                                (vector->list known-channels))
                           channel))
                 (state `(("channels" . ,known-channels)
                          ("feed" . (("items" . ,(or (ref data "items") #()))
                                     ("pagination" . ,(ref data "pagination"))
                                     ("name" . ,name)))
                          ("route" . (("name" . "PagePaginatedChannel")
                                      ("params" . (("channel" . ,channel)))))))
                 (html (render (feed-template state))))
            (cache-put! path html feed-ttl)
            (values good-response html)))))))

;; The home page always answers: if Featured cannot be loaded it still shows
;; the usage instructions, with a note instead of the feed.
(define (home-page)
  (display ";\tHandler: home-page")
  (let ((key "home"))
    (cached-page
      key
      (lambda ()
        (let* ((state (catch #t
                        (lambda ()
                          (receive (response body) (fetch "https://ifunny.co/")
                            (remember-cookies! response)
                            (display (string-append
                                       ";\tStatus: "
                                       (number->string (response-code response))))
                            (and (= (response-code response) 200)
                                 (initial-state body))))
                        (lambda (key . args)
                          (display (string-append ";\tUpstream: "
                                                  (symbol->string key)))
                          #f)))
               (html (catch #t
                       (lambda () (render (home-template state)))
                       (lambda _
                         (set! state #f)
                         (render (home-template #f))))))
          (cache-put! key html (if state feed-ttl home-failure-ttl))
          (values good-response html))))))

;; iFunny's own image service crops the 20px watermark strip off the bottom
;; of a picture and returns it in its original format, so nothing has to be
;; decoded here.  Videos are left alone: iFunny's GIFs are mp4 files stored
;; under /images/ too, and the image service refuses them.
(define (watermark-free-url url)
  (let ((uri (string->uri url)))
    (if (and uri
             (string-prefix? "/images/" (uri-path uri))
             (not (string-suffix? ".mp4" (uri-path uri))))
      (string-append "https://imageproxy.getfn.io/crop:x-20" (uri-path uri))
      url)))

(define (proxy-page query)
  (display ";\tHandler: proxy-page")
  (let ((resp "")
        (body "")
        (url (let ((raw (query-ref query "url")))
               (if raw (uri-decode raw) "")))
        (vid (and query (member "vid" (string-split query #\&)) #t)))
    (if (or (string-prefix? "https://img.ifunny.co/" url)
            (string-prefix? "https://imgb.ifunny.co/" url)
            (string-prefix? "https://img.ifnapp.com/" url)
            (string-prefix? "https://imgcltest10.ifunny.co/" url)
            (string-prefix? "https://img.getfn.io/" url)
            (string-prefix? "https://imageproxy.getfn.io/" url))
      (with-upstream-errors
       (lambda ()
        (receive (_resp _body)
                 (fetch (if vid url (watermark-free-url url)))
                 (set! resp _resp)
                 (set! body _body))
        (display (string-append ";\tStatus: "
                                (number->string (response-code resp))))
        (cond
          ((equal? (response-code resp) 200)
           (values (build-response
                     #:code 200
                     #:headers `((content-type .
                                  ,(response-content-type resp))
                                 (cache-control .
                                  ,(parse-header 'cache-control
                                                 "max-age=604800"))))
                   body))
          ((and (>= (response-code resp) 300) (<= (response-code resp) 399))
           (values (build-response
                     #:code (response-code resp)
                     #:headers `((location . ,(parse-header
                                                'location
                                                (uri-path
                                                  (response-location
                                                    resp))))
                                 (content-type .
                                  ,(response-content-type resp))))
                   "Redirect"))
          (else (error-page (upstream-status (response-code resp)))))))
      (error-page 400))))

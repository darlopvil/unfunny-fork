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

;; Where a redirect from iFunny points, as an absolute URI.  BASE is the URI
;; that was requested; Locations may be relative to it.  #f when there is no
;; usable Location.
(define (redirect-target response base)
  (let ((location (response-location response)))
    (cond ((not location) #f)
          ((uri-host location)
           (build-uri (or (uri-scheme location) (uri-scheme base))
                      #:host (uri-host location)
                      #:port (uri-port location)
                      #:path (uri-path location)
                      #:query (uri-query location)))
          ((string-prefix? "/" (uri-path location))
           (build-uri (uri-scheme base)
                      #:host (uri-host base)
                      #:path (uri-path location)
                      #:query (uri-query location)))
          (else #f))))

;; A redirect from iFunny's pages, followed within UNfunny: the visitor goes
;; to the same page on this instance.  Redirects off iFunny are refused.
(define (redirect response code base)
  (let* ((target (redirect-target response base))
         (path (and target
                    (local-path (uri->string
                                  (build-uri 'https
                                             #:host (uri-host target)
                                             #:path (uri-path target)))))))
    (if path
      (values (build-response
                #:code code
                #:headers `((location . ,(string->uri-reference
                                           (if (uri-query target)
                                             (string-append path "?"
                                                            (uri-query target))
                                             path)))))
              "Redirect")
      (error-page 502))))

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

;; shtml->html escapes '&' in text and leaves it as it is in attribute values,
;; where browsers read it fine, so the HTML is served as it comes.
(define (render shtml)
  (shtml->html shtml))

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
               (redirect resp (response-code resp) (string->uri url)))
              (else
               (error-page (upstream-status (response-code resp)))))))))

(define (post-page path)
  (display ";\tHandler: post-page")
  (generic-page post-template path post-ttl))

(define (channel-page path)
  (display ";\tHandler: channel-page")
  (generic-page (lambda (state) (feed-template (gather-channel state)))
                path feed-ttl))

(define (featured-page path)
  (display ";\tHandler: featured-page")
  (generic-page (lambda (state) (feed-template (gather-featured state)))
                path feed-ttl))

(define (tag-page path)
  (display ";\tHandler: tag-page")
  (generic-page (lambda (state) (feed-template (gather-tag state)))
                path feed-ttl))

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

(define (without key alist)
  (remove (lambda (entry) (equal? (car entry) key)) alist))

(define (feed-items state)
  (let ((items (ref state "feed" "items")))
    (if (vector? items) (vector->list items) '())))

;; STATE with its feed's items and pagination replaced.
(define (with-feed state items pagination)
  (acons "feed"
         (acons "items" (list->vector items)
                (acons "pagination" pagination
                       (without "items"
                                (without "pagination"
                                         (or (ref state "feed") '())))))
         (without "feed" state)))

;; STATE with the Featured page number its next page link counts from.
(define (with-featured-page state page)
  (let ((route (or (ref state "route") '())))
    (acons "route"
           (acons "params"
                  (acons "page" (number->string page)
                         (without "page" (or (ref route "params") '())))
                  (without "params" route))
           (without "route" state))))

;; iFunny's batches can overlap; keeps the first of each post.
(define (unique-posts items)
  (delete-duplicates items
                     (lambda (a b)
                       (let ((a (ref a "id")) (b (ref b "id")))
                         (and (string? a) (equal? a b))))))

;; iFunny's feed pages are small: 5 posts for tags, 6 for channels, 10 for
;; Featured and 1 to 3 for profiles.  Its web client loads further batches
;; as you scroll, and its API ignores any limit.  So each UNfunny page
;; gathers up to page-target posts, asking the API for at most
;; extra-batches more batches.  Any failure just stops gathering: what is
;; there is shown.
(define page-target 20)
(define extra-batches 3)

;; The API's next batch for a cursor feed ("items" and "pagination"), via
;; PATH-FOR, which builds the API path for a cursor.  #f at the end.
(define (cursor-batch path-for renew)
  (lambda (pagination)
    (let ((next (ref pagination "next")))
      (and (eq? (ref pagination "hasNext") #t)
           (non-empty-string? next)
           (api-get (path-for next) renew)))))

;; STATE with up to page-target posts, the extra ones from BATCH, which
;; takes the pagination so far.
(define (gather state batch)
  (let loop ((items (feed-items state))
             (pagination (ref state "feed" "pagination"))
             (batches 0))
    (let ((data (and (< (length items) page-target)
                     (< batches extra-batches)
                     (api-available?)
                     (catch #t
                       (lambda () (batch pagination))
                       (lambda _ #f)))))
      (if (and data
               (vector? (ref data "items"))
               (> (vector-length (ref data "items")) 0))
        (loop (append items (vector->list (ref data "items")))
              (ref data "pagination")
              (1+ batches))
        (with-feed state (unique-posts items) pagination)))))

(define (gather-tag state)
  (let ((tag (ref state "route" "params" "tag")))
    (if (non-empty-string? tag)
      (gather state
              (cursor-batch (lambda (next)
                              (string-append "/v1/feeds/tags/"
                                             (uri-encode tag) "?next=" next))
                            (renew-session
                              (string-append "/tags/" (uri-encode tag)))))
      state)))

(define (gather-channel state)
  (let ((channel (ref state "route" "params" "channel")))
    (if (non-empty-string? channel)
      (gather state
              (cursor-batch (lambda (next)
                              (string-append "/v1/feeds/channels/"
                                             (uri-encode channel)
                                             "?next=" next))
                            (renew-session
                              (string-append "/channels/"
                                             (uri-encode channel)))))
      state)))

(define (gather-user state)
  (let ((nick (ref state "user" "data" "nick")))
    (if (non-empty-string? nick)
      (gather state
              (cursor-batch (lambda (next)
                              (string-append "/v1/user/" (uri-encode nick)
                                             "/timeline/" next))
                            (renew-session
                              (string-append "/user/" (uri-encode nick)))))
      state)))

;; Featured pages go by number: an UNfunny page shows iFunny's page and as
;; many following ones as fit, and its next link counts from the last one.
(define (gather-featured state)
  (let ((pages (ref state "feed" "pageCount")))
    (let loop ((items (feed-items state))
               (page (or (let ((page (ref state "route" "params" "page")))
                           (and (string? page) (string->number page)))
                         1))
               (batches 0))
      (let ((data (and (< (length items) page-target)
                       (< batches extra-batches)
                       (integer? pages)
                       (< page pages)
                       (api-available?)
                       (catch #t
                         (lambda ()
                           (api-get (string-append "/v1/feeds/featured?page="
                                                   (number->string (1+ page)))
                                    (renew-session "/")))
                         (lambda _ #f)))))
        (if (and data
                 (vector? (ref data "items"))
                 (> (vector-length (ref data "items")) 0))
          (loop (append items (vector->list (ref data "items")))
                (1+ page)
                (1+ batches))
          (with-featured-page
            (with-feed state (unique-posts items) (ref state "feed" "pagination"))
            page))))))

(define (user-page path)
  (display ";\tHandler: user-page")
  (generic-page (lambda (state) (user-template (gather-user state)))
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
                 (html (render (feed-template (gather-channel state)))))
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
                       (lambda ()
                         (render (home-template (gather-featured state))))
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

;; The only hosts the media proxy fetches from: iFunny's media servers.
(define media-hosts
  '("img.ifunny.co" "imgb.ifunny.co" "img.ifnapp.com" "imgcltest10.ifunny.co"
    "img.getfn.io" "imageproxy.getfn.io"))

(define (media-uri? uri)
  (and uri
       (eq? (uri-scheme uri) 'https)
       (not (uri-userinfo uri))
       (not (uri-port uri))
       (member (uri-host uri) media-hosts)
       #t))

;; Fetches a media file, following up to 3 redirects as long as they stay
;; on iFunny's media hosts.  Returns the final response and its body.
(define (fetch-media uri)
  (let loop ((uri uri) (redirects 0))
    (receive (response body) (fetch (uri->string uri))
      (let ((code (response-code response)))
        (if (and (>= code 300) (<= code 399))
          (let ((target (redirect-target response uri)))
            (if (and target (media-uri? target) (< redirects 3))
              (loop target (1+ redirects))
              (throw 'upstream-error 'redirect)))
          (values response body))))))

(define (proxy-page query)
  (display ";\tHandler: proxy-page")
  (let* ((url (let ((raw (query-ref query "url")))
                (and raw (false-if-exception (uri-decode raw)))))
         (uri (and url (string->uri url)))
         (vid (and query (member "vid" (string-split query #\&)) #t)))
    (if (media-uri? uri)
      (with-upstream-errors
        (lambda ()
          (receive (resp body)
              (fetch-media (if vid uri (string->uri (watermark-free-url url))))
            (display (string-append ";\tStatus: "
                                    (number->string (response-code resp))))
            (if (= (response-code resp) 200)
              (values (build-response
                        #:code 200
                        #:headers `((content-type .
                                     ,(response-content-type resp))
                                    (cache-control .
                                     ,(parse-header 'cache-control
                                                    "max-age=604800"))))
                      body)
              (error-page (upstream-status (response-code resp)))))))
      (error-page 400))))

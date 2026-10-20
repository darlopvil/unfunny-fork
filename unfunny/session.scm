;; Copyright (C) 2026 darlopvil (unfunny-fork)
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

;; iFunny's JSON API, through the anonymous session it gives every visitor.
;;
;; iFunny's web client loads further pages of channels and profiles from
;; https://ifunny.co/api/v1/..., which answers 401 without the anonymous
;; session that any of iFunny's HTML pages sets: the CID, F_CID and
;; x-csrf-token cookies.  UNfunny keeps the cookies of the pages it fetches
;; anyway, so the session costs no extra request, and sends them with API
;; calls the way a browser does, x-csrf-token also as a header.
;;
;; When the API keeps refusing, api-available? turns false for an hour, so
;; the links that depend on it disappear instead of leading to error pages.

(define-module (unfunny session)
  #:use-module (unfunny upstream)
  #:use-module (web response)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:export (remember-cookies!
            api-available?
            api-get))

(define session-cookie-names '("CID" "F_CID" "x-csrf-token"))

;; How long the API-dependent links stay hidden after the API refused us.
(define api-pause 3600)

(define lock (make-mutex))
(define cookies '())          ; alist: name -> value
(define broken-until 0)

;; "CID=abc; Path=/; HttpOnly" -> ("CID" . "abc"), or #f for other cookies.
(define (parse-set-cookie header)
  (let* ((pair (car (string-split header #\;)))
         (i (string-index pair #\=)))
    (and i
         (let ((name (string-trim-both (substring pair 0 i)))
               (value (string-trim-both (substring pair (1+ i)))))
           (and (member name session-cookie-names)
                (not (string-null? value))
                (cons name value))))))

;; Keeps the session cookies RESPONSE sets, if any.
(define (remember-cookies! response)
  (let ((found (filter-map (lambda (header)
                             (and (eq? (car header) 'set-cookie)
                                  (string? (cdr header))
                                  (parse-set-cookie (cdr header))))
                           (response-headers response))))
    (unless (null? found)
      (with-mutex lock
        (set! cookies
              (append found
                      (remove (lambda (cookie) (assoc (car cookie) found))
                              cookies)))))))

(define (session-ready?)
  (with-mutex lock
    (every (lambda (name) (assoc name cookies)) session-cookie-names)))

(define (forget-session!)
  (with-mutex lock (set! cookies '())))

(define (api-available?)
  (>= (current-time) broken-until))

(define (pause-api!)
  (set! broken-until (+ (current-time) api-pause)))

;; One API call -> its status code and body.
(define (call path)
  (let* ((current (with-mutex lock cookies))
         (token (or (assoc-ref current "x-csrf-token") "")))
    (receive (response body)
        (fetch (string-append "https://ifunny.co/api" path)
               #:headers
               `((accept (application/json))
                 (x-requested-with . "fetch")
                 (x-csrf-token . ,token)
                 (cookie . ,(string-join
                              (map (lambda (cookie)
                                     (string-append (car cookie) "="
                                                    (cdr cookie)))
                                   current)
                              "; "))))
      (values (response-code response) body))))

;; The parsed JSON the API answers for PATH ("/v1/..."), or throws
;; 'api-error.  RENEW is a thunk that fetches one of iFunny's HTML pages
;; through a path that calls remember-cookies!, which renews the session.
;; It runs when there is no session yet, and when the API answers 401; the
;; call is then retried once.
(define (api-get path renew)
  (unless (session-ready?)
    (renew))
  (receive (code body) (call path)
    (receive (code body)
        (if (= code 401)
          (begin
            (forget-session!)
            (renew)
            (call path))
          (values code body))
      (display (string-append ";\tAPI: " (number->string code)))
      (unless (= code 200)
        (when (memv code '(401 403 429))
          (pause-api!))
        (throw 'api-error code))
      (json-string->scm (if (bytevector? body) (utf8->string body) body)))))

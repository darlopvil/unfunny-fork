;; Copyright (C) 2024-2025 Skylar Astaroth <cobra@vern.cc>
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
  #:use-module (unfunny templates)
  #:use-module (unfunny vips)
  #:use-module (htmlprag)
  #:use-module (web response)
  #:use-module (web client)
  #:use-module (web http)
  #:use-module (web uri)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 string-fun)
  #:use-module ((rnrs) #:prefix rnrs:)
  #:use-module (system foreign)
  #:export (error-page
            index-page
            picture-page
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
  (display ";\tHandler: error-page") (newline)
  (values (build-response
            #:code code
            #:headers `((content-type . (text/html))))
          (shtml->html (error-template code))))

(define (generic-page procedure path)
  (let ((resp "")
        (body "")
        (sub
          (cond
            ((equal? (substring path 0 4) "/br/") "br.")
            (else ""))))
    (receive (_resp _body)
             (http-request (string-append
                             "https://" sub "ifunny.co"
                             (if (equal? (substring path 0 4) "/br/")
                               (substring path 3) path)))
             (set! resp _resp)
             (set! body _body))
    (display (string-append ";\tStatus: "
                            (number->string (response-code resp))))
    (newline)
    (cond ((equal? (response-code resp) 200)
           (let ((doc (html->sxml-0nf body #:strict? #t)))
             (values good-response
                     (string-replace-substring
                       (shtml->html (procedure doc))
                       "&amp;" "&"))))
          ((and (>= (response-code resp) 300) (<= (response-code resp) 399))
           (redirect resp (response-code resp)))
          (else (error-page (response-code resp))))))

(define (picture-page path)
  (display ";\tHandler: picture-page")
  (generic-page picture-template path))

(define (index-page host)
  (display ";\tHandler: index-page") (newline)
  (values good-response (shtml->html (index-template host))))

(define (crop image)
  (if (not (file-exists? "/tmp/unfunny"))
    (mkdir "/tmp/unfunny"))
  (define in (vips-image-new-from-buffer
               (bytevector->pointer image)
               (array-length image)
               (string->pointer "")
               %null-pointer))
  (define out (bytevector->pointer (rnrs:make-bytevector 0)))
  (vips-extract-area in out 0 0 (vips-image-get-width in)
                     (- (vips-image-get-height in) 20) %null-pointer)
  (define buf (bytevector->pointer (rnrs:make-bytevector 0)))
  (define len (rnrs:make-bytevector (sizeof size_t)))
  (vips-pngsave-buffer (dereference-pointer out)
                       buf (bytevector->pointer len) %null-pointer)
  (pointer->bytevector (dereference-pointer buf)
                       (rnrs:bytevector-u64-native-ref len 0)))

(define (proxy-page query)
  (display ";\tHandler: proxy-page") (newline)
  (let ((resp "")
        (body "")
        (url ""))
    (map (lambda (p)
           (if (equal? (car p) "url")
             (set! url (uri-decode (cadr p)))))
         (map (lambda (s)
                (string-split s #\=))
              (string-split query #\&)))
    (if (or
          (and
            (>= (string-length url) 22)
            (equal? (substring url 0 22) "https://img.ifunny.co/"))
          (and
            (>= (string-length url) 23)
            (equal? (substring url 0 23) "https://imgb.ifunny.co/"))
          (and
            (>= (string-length url) 30)
            (equal? (substring url 0 30) "https://imgcltest10.ifunny.co/")))
      (begin
        (receive (_resp _body) (http-request url)
                 (set! resp _resp)
                 (set! body _body))
        (cond
          ((equal? (response-code resp) 200)
           (values (build-response
                     #:code 200
                     #:headers `((content-type .
                                  ,(response-content-type resp))
                                 (cache-control .
                                  ,(parse-header 'cache-control
                                                 "max-age=604800"))))
                   (crop body)))
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
          (else (error-page 404))))
      (error-page 400))))

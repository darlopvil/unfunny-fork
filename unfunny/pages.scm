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
  #:use-module (unfunny templates)
  #:use-module (htmlprag)
  #:use-module (web response)
  #:use-module (web client)
  #:use-module (web http)
  #:use-module (web uri)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 string-fun)
  #:export (error-page
            index-page
            picture-page
            video-page
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
        (url (if (string-prefix? "/br/" path)
               (string-append "https://br.ifunny.co" (string-drop path 3))
               (string-append "https://ifunny.co" path))))
    (receive (_resp _body)
             (http-request url)
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

(define (video-page path)
  (display ";\tHandler: video-page")
  (generic-page video-template path))

(define (index-page host)
  (display ";\tHandler: index-page") (newline)
  (values good-response (shtml->html (index-template host))))

;; iFunny's own image service crops the 20px watermark strip off the bottom
;; of a picture and returns it in its original format, so nothing has to be
;; decoded here.
(define (watermark-free-url url)
  (let ((uri (string->uri url)))
    (if (and uri (string-prefix? "/images/" (uri-path uri)))
      (string-append "https://imageproxy.getfn.io/crop:x-20" (uri-path uri))
      url)))

(define (proxy-page query)
  (display ";\tHandler: proxy-page") (newline)
  (let ((resp "")
        (body "")
        (url "")
        (vid #f))
    (map (lambda (p)
           (if (equal? (car p) "url")
             (set! url (uri-decode (cadr p))))
           (if (equal? (car p) "vid")
             (set! vid #t)))
         (map (lambda (s)
                (string-split s #\=))
              (string-split query #\&)))
    (if (or (string-prefix? "https://img.ifunny.co/" url)
            (string-prefix? "https://imgb.ifunny.co/" url)
            (string-prefix? "https://img.ifnapp.com/" url)
            (string-prefix? "https://imgcltest10.ifunny.co/" url)
            (string-prefix? "https://img.getfn.io/" url))
      (begin
        (receive (_resp _body)
                 (http-request (if vid url (watermark-free-url url)))
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
          (else (error-page 404))))
      (error-page 400))))

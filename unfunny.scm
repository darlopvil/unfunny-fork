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

(add-to-load-path (dirname (current-filename)))

(define-module (unfunny)
  #:use-module (unfunny pages)
  #:use-module (unfunny server)
  #:use-module (web uri)
  #:use-module (web request)
  #:use-module (web http)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 binary-ports))

(define (handler request request-body)
  (let ((uri (request-uri request))
        (path (uri-path (request-uri request)))
        ;; #f when the path holds broken percent-encoding ("/picture/%zz").
        (path-components
          (false-if-exception
            (split-and-decode-uri-path
              (uri-path
                (request-uri request))))))

    (display (string-append
               (strftime "%c" (localtime (current-time)))
               ": " (uri->string uri)))
    (cond
      ((not path-components)
       (error-page 400))
      ((equal? path "/style.css")
       (display ";\tHandler: internal") (newline)
       (values `((content-type . (text/css)))
               (call-with-input-file "static/style.css" get-string-all)))
      ((equal? path "/favicon.png")
       (display ";\tHandler: internal") (newline)
       (values `((content-type . (image/png))
                 (cache-control .
                  ,(parse-header 'cache-control
                                 "max-age=604800")))
               (call-with-input-file "static/logo.png" get-bytevector-all)))
      ((equal? path "/proxy")
       (proxy-page (uri-query uri)))
      ((equal? path "/")
       (home-page))
      ((and (= (length path-components) 2)
            (equal? (car path-components) "channels"))
       (channel-page path))
      ;; Posts, at any of iFunny's paths for them (also under /br/ for iFunny
      ;; Brazil).  The page shows each post according to its real type.
      ((let ((parts (if (and (pair? path-components)
                             (equal? (car path-components) "br"))
                      (cdr path-components)
                      path-components)))
         (and (>= (length parts) 2)
              (member (car parts) '("picture" "meme" "video" "gif"))))
       (post-page path))
      (else (error-page 404)))))

(let ((port (if (getenv "PORT")
              (string->number (getenv "PORT"))
              8004))
      (sock (socket PF_INET SOCK_STREAM 0)))
  (setsockopt sock SOL_SOCKET SO_REUSEPORT 1)
  (bind sock AF_INET INADDR_ANY port)
  (run-threaded-server handler sock))

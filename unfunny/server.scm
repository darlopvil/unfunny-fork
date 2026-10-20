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

;; Threaded HTTP server.
;;
;; Guile's built-in 'http server handles one request at a time, so a single
;; slow upstream request stalls every other visitor.  This loop serves each
;; connection in its own thread, up to max-clients at once, and reuses
;; handle-request from (web server) for error handling and response sanitizing.
;;
;; Each request's log output is collected and written as one line, so lines
;; from concurrent requests never interleave, and flushed right away so it
;; shows up in container logs immediately.

(define-module (unfunny server)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:export (run-threaded-server))

(define max-clients 32)

(define slots-lock (make-mutex))
(define slot-freed (make-condition-variable))
(define active 0)

(define (acquire-slot!)
  (with-mutex slots-lock
    (let wait ()
      (when (>= active max-clients)
        (wait-condition-variable slot-freed slots-lock)
        (wait)))
    (set! active (1+ active))))

(define (release-slot!)
  (with-mutex slots-lock
    (set! active (1- active))
    (signal-condition-variable slot-freed)))

(define log-lock (make-mutex))

(define (write-log! line)
  (unless (string-null? line)
    (with-mutex log-lock
      (display line)
      (unless (string-suffix? "\n" line)
        (newline))
      (force-output))))

(define (close-connection response)
  (build-response #:version (response-version response)
                  #:code (response-code response)
                  #:reason-phrase (response-reason-phrase response)
                  #:headers (cons '(connection close)
                                  (alist-delete 'connection
                                                (response-headers response)))))

(define (send! port response body)
  (let ((response (write-response (close-connection response) port)))
    (when body
      (write-response-body response body))
    (force-output port)))

(define (serve-client handler port)
  (let ((log (open-output-string)))
    (catch #t
      (lambda ()
        (setvbuf port 'block)
        (let ((request (catch #t
                         (lambda () (read-request port))
                         (const #f))))
          (if request
            (receive (response body _)
                (parameterize ((current-output-port log))
                  (handle-request handler request
                                  (read-request-body request) '()))
              (send! port response body))
            (send! port (build-response #:code 400) #f))))
      (lambda (key . args)
        (format log " (connection error: ~a)" key)))
    (catch #t (lambda () (close-port port)) (const #f))
    (write-log! (get-output-string log))))

(define (run-threaded-server handler sock)
  ;; A client that goes away mid-response (a closed tab) makes the next write
  ;; raise SIGPIPE, whose default action kills the whole process.  Ignoring it
  ;; turns that into an EPIPE error that only ends that client's thread.
  ;; Guile's own 'http server does the same in http-open.
  (sigaction SIGPIPE SIG_IGN)
  (listen sock 128)
  (let loop ()
    (acquire-slot!)
    (let ((client (catch 'system-error
                    (lambda () (car (accept sock)))
                    (lambda args
                      (release-slot!)
                      #f))))
      (when client
        (call-with-new-thread
          (lambda ()
            (dynamic-wind
              (const #t)
              (lambda () (serve-client handler client))
              release-slot!)))))
    (loop)))

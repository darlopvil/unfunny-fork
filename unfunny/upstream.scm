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

;; Upstream requests with bounded time.
;;
;; Guile's http-request sets no timeouts, and GnuTLS reads the raw socket from
;; C, where a silent peer blocks forever (SO_RCVTIMEO is not enough: the C read
;; path just polls again).  So every upstream request gets its own socket with
;; a connect timeout, and a watchdog thread shuts down any socket that outlives
;; its deadline, which wakes up whatever is blocked on it.

(define-module (unfunny upstream)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (ice-9 threads)
  #:export (fetch))

(define connect-timeout 5)    ; seconds per connect() attempt (SO_SNDTIMEO)
(define request-deadline 15)  ; seconds per request once connected

;; (web client) does not export tls-wrap, but reusing it keeps Guile's own TLS
;; setup and certificate verification instead of duplicating them here.
(define tls-wrap (@@ (web client) tls-wrap))

(define watch-lock (make-mutex))
(define watched '())          ; list of (deadline . socket)
(define watchdog-started? #f)

;; The watchdog thread is started on first use: starting a thread while this
;; module is still being loaded deadlocks on Guile's module loading lock.
(define (watch! sock)
  (with-mutex watch-lock
    (unless watchdog-started?
      (set! watchdog-started? #t)
      (call-with-new-thread watchdog))
    (set! watched (cons (cons (+ (current-time) request-deadline) sock)
                        watched))))

(define (unwatch! sock)
  (with-mutex watch-lock
    (set! watched (filter (lambda (entry) (not (eq? (cdr entry) sock)))
                          watched))))

(define (watchdog)
  (sleep 1)
  (let ((now (current-time)))
    (with-mutex watch-lock
      (set! watched
            (filter (lambda (entry)
                      (if (>= now (car entry))
                        (begin
                          (catch #t
                            (lambda () (shutdown (cdr entry) 2))
                            (const #f))
                          #f)
                        #t))
                    watched))))
  (watchdog))

;; connect() returns #f when SO_SNDTIMEO expires, and throws when the
;; connection is refused or unreachable.
(define (connect-any uri)
  (let loop ((addresses (if (uri-port uri)
                          (getaddrinfo (uri-host uri)
                                       (number->string (uri-port uri))
                                       AI_NUMERICSERV)
                          (getaddrinfo (uri-host uri) "https")))
             (timed-out? #f))
    (let* ((ai (car addresses))
           (sock (socket (addrinfo:fam ai) SOCK_STREAM IPPROTO_IP)))
      (setsockopt sock SOL_SOCKET SO_SNDTIMEO (cons connect-timeout 0))
      (let ((result (catch 'system-error
                      (lambda () (if (connect sock (addrinfo:addr ai))
                                   'connected
                                   'timeout))
                      (const 'failed))))
        (if (eq? result 'connected)
          sock
          (let ((timed-out? (or timed-out? (eq? result 'timeout))))
            (close-port sock)
            (if (null? (cdr addresses))
              (throw (if timed-out? 'upstream-timeout 'upstream-error)
                     'connect)
              (loop (cdr addresses) timed-out?))))))))

;; Same contract as (http-request url): returns the response and its body.
;; Every failure is rethrown as 'upstream-timeout when a timeout or the
;; deadline was hit, and as 'upstream-error otherwise, with the original
;; error key as its argument.
(define (fetch url)
  (let ((start (current-time)))
    (catch #t
      (lambda ()
        (let* ((uri (string->uri url))
               (sock (connect-any uri)))
          (watch! sock)
          (dynamic-wind
            (const #t)
            (lambda ()
              (setvbuf sock 'block 65536)
              (http-request uri #:port (tls-wrap sock (uri-host uri))))
            (lambda ()
              (unwatch! sock)
              (close-port sock)))))
      (lambda (key . args)
        (cond
          ((memq key '(upstream-timeout upstream-error))
           (apply throw key args))
          ((>= (- (current-time) start) request-deadline)
           (throw 'upstream-timeout key))
          (else
           (throw 'upstream-error key)))))))

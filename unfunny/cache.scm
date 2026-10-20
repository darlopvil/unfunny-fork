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

;; Small in-memory cache of rendered pages, so that loading the same page again
;; does not cost another request to iFunny.  It holds UNfunny's own HTML (a few
;; KB per page, not iFunny's 40-80 KB), with a hard cap on the number of entries
;; so it stays small on low-memory hosts.
;;
;; Visitors asking for the same uncached page at the same time share a single
;; request to iFunny (shared-flight): the first one fetches it and the others
;; wait for its answer, instead of each sending its own.

(define-module (unfunny cache)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-9)
  #:export (cache-ref cache-put! shared-flight))

(define max-entries 500)

(define lock (make-mutex))
(define table (make-hash-table))   ; key -> (expiry . value)

;; The value stored under KEY, or #f if there is none or it has expired.
(define (cache-ref key)
  (with-mutex lock
    (let ((entry (hash-ref table key)))
      (cond ((not entry) #f)
            ((> (car entry) (current-time)) (cdr entry))
            (else (hash-remove! table key) #f)))))

;; Stores VALUE under KEY for TTL seconds.
(define (cache-put! key value ttl)
  (with-mutex lock
    (unless (hash-ref table key)
      (when (>= (hash-count (const #t) table) max-entries)
        (make-room!)))
    (hash-set! table key (cons (+ (current-time) ttl) value))))

;; Called with the lock held.  Drops expired entries, then, if the cache is
;; still full, the quarter of entries closest to expiring.
(define (make-room!)
  (let* ((now (current-time))
         (entries (hash-map->list cons table)))
    (for-each (lambda (entry)
                (when (<= (cadr entry) now)
                  (hash-remove! table (car entry))))
              entries)
    (when (>= (hash-count (const #t) table) max-entries)
      (let ((by-expiry (sort (hash-map->list cons table)
                             (lambda (a b) (< (cadr a) (cadr b))))))
        (for-each (lambda (entry) (hash-remove! table (car entry)))
                  (list-head by-expiry (quotient max-entries 4)))))))

(define-record-type <flight>
  (make-flight result)
  flight?
  (result flight-result set-flight-result!))   ; #f until done, then a list

(define flights-lock (make-mutex))
(define flight-done (make-condition-variable))
(define flights (make-hash-table))   ; key -> flight in progress

;; Runs THUNK, which returns a response and its body, unless a call with the
;; same KEY is already running: then waits for that one and returns its
;; values.  THUNK must not throw; if it does anyway, its waiters get FALLBACK,
;; a thunk returning a response and body too.
(define (shared-flight key thunk fallback)
  (let* ((joined? #f)
         (flight (with-mutex flights-lock
                   (let ((running (hash-ref flights key)))
                     (if running
                       (begin (set! joined? #t) running)
                       (let ((flight (make-flight #f)))
                         (hash-set! flights key flight)
                         flight))))))
    (if joined?
      (with-mutex flights-lock
        (let wait ()
          (unless (flight-result flight)
            (wait-condition-variable flight-done flights-lock)
            (wait)))
        (display ";\tShared")
        (apply values (flight-result flight)))
      (let ((result (catch #t
                      (lambda () (call-with-values thunk list))
                      (lambda _ (call-with-values fallback list)))))
        (with-mutex flights-lock
          (set-flight-result! flight result)
          (hash-remove! flights key)
          (broadcast-condition-variable flight-done))
        (apply values result)))))

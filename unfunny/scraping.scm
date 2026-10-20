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

;; Reading iFunny pages.
;;
;; iFunny renders its pages from a JSON state it embeds in every page as
;; window.__INITIAL_STATE__.  Reading it is far more robust than walking the
;; DOM through absolute element paths, and it holds everything about a post:
;; its type, title, original file, video poster, author, tags and counters.
;; Post pages and feeds share the same structure: the posts are in
;; feed.items, and a post page is a feed of one.

(define-module (unfunny scraping)
  #:use-module (json)
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:export (initial-state
            ref
            post-item
            post-title
            non-empty-string?
            video-post?
            gif-post?
            local-path
            proxy))

(define state-marker "window.__INITIAL_STATE__=")

;; The parsed state of an iFunny page.  The JSON is followed by more script
;; in the same <script> element, so it is read as a single value from the
;; marker on instead of being cut out first.
(define (initial-state body)
  (let ((start (string-contains body state-marker)))
    (unless start
      (throw 'initial-state-missing))
    (json->scm (open-input-string
                 (substring body (+ start (string-length state-marker))))
               #:concatenated #t)))

;; Nested lookup: string keys index JSON objects (alists), numbers index
;; arrays (vectors).  Returns #f as soon as something is missing.
(define (ref data . keys)
  (fold (lambda (key data)
          (cond ((not data) #f)
                ((and (string? key) (list? data)) (assoc-ref data key))
                ((and (integer? key) (vector? data) (< key (vector-length data)))
                 (vector-ref data key))
                (else #f)))
        data
        keys))

;; The post of a post page.  Throws when there is none, so the page becomes
;; a 502 instead of an empty template.
(define (post-item state)
  (let ((item (ref state "feed" "items" 0)))
    (unless (and item (string? (ref item "url")))
      (throw 'post-missing))
    item))

(define (non-empty-string? value)
  (and (string? value) (not (string-null? value))))

;; Many posts have no title.  iFunny then uses a generated one ("Video memes
;; <id> by <nick>"), which is better than nothing.
(define (post-title item)
  (find non-empty-string?
        (list (ref item "title")
              (ref item "fixedTitle")
              (ref item "id")
              "Untitled")))

;; Videos and iFunny's GIFs are mp4 files; everything else (picture,
;; text_pic...) is an image.  Deciding by the file rather than by the type
;; keeps working when iFunny adds another variant of either.
(define (video-post? item)
  (let ((uri (and (string? (ref item "url")) (string->uri (ref item "url")))))
    (and uri (string-suffix? ".mp4" (uri-path uri)))))

;; "gif", "text_gif"...
(define (gif-post? item)
  (let ((type (ref item "type")))
    (and (string? type) (string-contains type "gif") #t)))

;; "https://ifunny.co/picture/..." -> "/picture/...", and iFunny Brazil's
;; "https://br.ifunny.co/..." -> "/br/...".  #f for anything else.
(define (local-path url)
  (let ((uri (and (string? url) (string->uri url))))
    (and uri
         (cond ((equal? (uri-host uri) "ifunny.co") (uri-path uri))
               ((equal? (uri-host uri) "br.ifunny.co")
                (string-append "/br" (uri-path uri)))
               (else #f)))))

;; Characters left as they are in the url parameter of proxy links.  ':' and
;; '/' are safe inside a query and keep the links readable; everything else
;; outside this set is percent-encoded, so a media URL with its own '?', '&',
;; '+' or '%' reaches the proxy unchanged.
(define url-parameter-chars
  (string->char-set
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~:/"))

;; Link to URL through the media proxy.  VIDEO? passes the file through as
;; it is: only pictures go through iFunny's watermark crop.
(define* (proxy url #:optional video?)
  (string-append (if video? "/proxy?vid&url=" "/proxy?url=")
                 (uri-encode url #:unescaped-chars url-parameter-chars)))

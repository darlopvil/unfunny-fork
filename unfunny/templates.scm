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

(define-module (unfunny templates)
  #:use-module (unfunny scraping)
  #:use-module (htmlprag)
  #:export (error-template
            index-template
            post-template))

(define (html-head title)
  `(head (title ,title " - UNfunny")
         (meta (@ (charset "UTF-8")))
         (meta (@ (name "viewport") (content "width=device-width")))
         (link (@ (rel "stylesheet") (href "/style.css")))
         (link (@ (rel "icon") (type "image/png") (href "/favicon.png")))))

(define heading
  `(header))

;; Value of the environment variable NAME, or #f when it is unset or empty.
(define (env name)
  (let ((value (getenv name)))
    (and value (not (string-null? value)) value)))

;; The AGPL requires offering users the source of the code the instance runs.
;; Anyone running a modified copy points SOURCE_URL at their own source.
(define source-url
  (or (env "SOURCE_URL") "https://github.com/darlopvil/unfunny-fork"))

(define footer
  `(footer
     (p (a (@ (href ,source-url))
           "Source Code"))
     ,(if (env "PATCHES_URL")
        `(p (a (@ (href ,(env "PATCHES_URL")))
               "Patches"))
        `())))

(define (index-template host)
  `(html ,(html-head "UNfunny")
         (body
           ,heading
           (h1 "UNfunny")
           (h4 "iFunny? more like UNfunny!!!!")
           (p "UNfunny is a privacy-respecting front-end to iFunny.")
           (br)
           (h2 "Usage")
           (p "You can use UNfunny by replacing " (code "ifunny.com")
              " with this website's domain")
           (h3 "Example")
           (pre (code
                  "https://ifunny.co/picture/get-how-the-"
                  "orangutans-have-gun-in-their-exhibit-why-aPrZa5Al9"))
           (p "becomes")
           (pre (code
                  "https://" ,host "/picture/get-how-the-"
                  "orangutans-have-gun-in-their-exhibit-why-aPrZa5Al9"))
           (br)
           (p "For iFunny Brazil links, replace the domain as usual, but "
              "add /br to the beginning of the path")
           (h3 "Example")
           (pre (code "https://br.ifunny.co/picture/WEatpqh3B"))
           (p "becomes")
           (pre (code "https://" ,host "/br/picture/WEatpqh3B"))
           ,footer)))

;; STATE is the page's parsed __INITIAL_STATE__ (see (unfunny scraping)).
(define (post-template state)
  (let ((item (post-item state)))
    (if (video-post? item)
      (video-template item)
      (picture-template item))))

(define (picture-template item)
  (let ((title (post-title item)))
    `(html ,(html-head title)
           (body
             ,heading
             (h1 ,title)
             (img (@ (alt ,title)
                     (src ,(proxy (ref item "url")))))
             ,footer))))

;; Videos never autoplay: browsers that block autoplay, even muted, must
;; work without the visitor changing any setting.  Nothing but the poster is
;; downloaded until the visitor presses play.  iFunny's GIFs are mp4 files
;; too: they play muted and loop, so once started they behave like a GIF.
(define (video-template item)
  (let ((title (post-title item))
        (poster (ref item "screen")))
    `(html ,(html-head title)
           (body
             ,heading
             (h1 ,title)
             (video (@ (aria-label ,title)
                       (controls)
                       (loop "loop")
                       ,@(if (gif-post? item) '((muted)) '())
                       (preload "none")
                       ,@(if (non-empty-string? poster)
                           `((poster ,(proxy poster #t)))
                           '())
                       (src ,(proxy (ref item "url") #t))))
             ,footer))))

(define (error-message code)
  (case code
    ((400) "Bad request.")
    ((404) "Not found.")
    ((502) "iFunny could not be reached, or sent something UNfunny could not read.")
    ((503) "iFunny is refusing requests from this instance right now. Try again later.")
    ((504) "iFunny took too long to answer.")
    (else #f)))

(define (error-template code)
  `(html ,(html-head (number->string code))
         (body
           ,heading
           (h1 ,(number->string code))
           ,(let ((message (error-message code)))
              (if message `(p ,message) '()))
           ,footer)))

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
  #:use-module (sxml xpath)
  #:use-module (htmlprag)
  #:export (error-template
            index-template
            picture-template
            video-template))

(define (html-head title)
  `(head (title ,title " - UNfunny")
         (meta (@ (charset "UTF-8")))
         (meta (@ (name "viewport") (content "width=device-width")))
         (link (@ (rel "stylesheet") (href "/style.css")))
         (link (@ (rel "icon") (type "image/png") (href "/favicon.png")))))

(define heading
  `(header))

(define footer
  `(footer
     (p (a (@ (href "https://git.vern.cc/cobra/UNfunny"))
           "Source Code"))
     ,(if (getenv "PATCHES_URL")
        `(p (a (@ (href ,(getenv "PATCHES_URL")))
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

(define (picture-template d)
  (let* ((pref '(html body div main div div div div img @))
         (src (car ((sxpath (append pref '(src *text*))) d)))
         (alt (car ((sxpath (append pref '(alt *text*))) d))))
    `(html ,(html-head alt)
           (body
             ,heading
             (h1 ,alt)
             (img (@ (alt ,alt)
                     (src ,(string-append
                             "/proxy?url="
                             src))))
             ,footer))))

(define (video-template d)
  (let* ((pref '(html body div main div div div div div video @))
         (src (car ((sxpath (append pref '(data-src *text*))) d)))
         (poster (car ((sxpath (append pref '(data-poster *text*))) d)))
         (alt (car ((sxpath (append pref '(aria-label *text*))) d))))
    `(html ,(html-head alt)
           (body
             ,heading
             (h1 ,alt)
             (video (@ (alt ,alt)
		       (loop "loop")
		       (controls)
		       (preload "auto")
		       (poster ,(string-append
                                  "/proxy?vid&url="
                                  poster))
                       (src ,(string-append
                               "/proxy?vid&url="
                               src))))
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

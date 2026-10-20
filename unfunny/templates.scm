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
  #:use-module (web uri)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:export (error-template
            home-template
            feed-template
            user-template
            post-template))

(define (html-head title)
  `(head (title ,title " - UNfunny")
         (meta (@ (charset "UTF-8")))
         (meta (@ (name "viewport") (content "width=device-width")))
         (link (@ (rel "stylesheet") (href "/style.css")))
         (link (@ (rel "icon") (type "image/png") (href "/favicon.png")))))

(define heading
  `(header (a (@ (class "brand") (href "/")) "UNfunny")))

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
           "Source Code")
        " · Fork of "
        (a (@ (href "https://codeberg.org/cobra/UNfunny"))
           "UNfunny")
        " by Skylar Astaroth")
     ,(if (env "PATCHES_URL")
        `(p (a (@ (href ,(env "PATCHES_URL")))
               "Patches"))
        `())))

;; Links to Featured and to every channel iFunny lists in its pages.
(define (channel-nav state)
  (let ((channels (ref state "channels")))
    `(nav (@ (class "channels"))
          (a (@ (href "/")) "Featured")
          ,@(if (vector? channels)
              (filter-map
                (lambda (channel)
                  (let ((name (ref channel "name"))
                        (path (ref channel "path")))
                    (and (non-empty-string? name)
                         (non-empty-string? path)
                         `(a (@ (href ,(string-append "/channels/" path)))
                             ,name))))
                (vector->list channels))
              '()))))

;; A post in a feed: its preview, linking to its UNfunny page.  Previews are
;; iFunny's 320px thumbnails (the watermark already cropped off pictures),
;; never videos.  #f for posts that cannot be linked or shown.
(define (feed-card item)
  (let ((link (local-path (ref item "canonical")))
        (thumb (ref item "thumb" "p"))
        (width (ref item "thumb" "pSize" "w"))
        (height (ref item "thumb" "pSize" "h"))
        (title (post-title item)))
    (and link
         (non-empty-string? thumb)
         `(a (@ (class "card") (href ,link))
             (img (@ (src ,(proxy thumb))
                     (alt ,title)
                     (loading "lazy")
                     ,@(if (and (integer? width) (integer? height))
                         `((width ,(number->string width))
                           (height ,(number->string height)))
                         '())))
             ,@(if (video-post? item)
                 `((span (@ (class "kind"))
                         ,(if (gif-post? item) "GIF" "Video")))
                 '())
             (span (@ (class "title")) ,title)))))

;; Where the next page of a feed is, following iFunny's own routes for them:
;; /page<N> for Featured, /tags/<tag>/<cursor> and /user/<nick>/timeline/
;; <cursor>.  Those routes only take numeric cursors, the ones tags and
;; profiles use.  #f when there is no next page.
(define (next-page-path state)
  (let* ((route (ref state "route" "name"))
         (params (ref state "route" "params"))
         (next (ref state "feed" "pagination" "next"))
         (cursor? (lambda (pattern)
                    (and (eq? (ref state "feed" "pagination" "hasNext") #t)
                         (string? next)
                         (string-match pattern next)))))
    (cond
      ((member route '("PageFeed" "PagePaginatedFeed"))
       (let ((page (or (and (string? (ref params "page"))
                            (string->number (ref params "page")))
                       1))
             (pages (ref state "feed" "pageCount")))
         (and (integer? pages)
              (< page pages)
              (string-append "/page" (number->string (1+ page))))))
      ((and (member route '("PageTag" "PagePaginatedTag"))
            (non-empty-string? (ref params "tag"))
            (cursor? "^[0-9]{10}$"))
       (string-append "/tags/" (uri-encode (ref params "tag")) "/" next))
      ((and (member route '("PageUser" "PagePaginatedUser"))
            (non-empty-string? (ref params "nick"))
            (cursor? "^[0-9]{10}(\\.[0-9]{1,5})?$"))
       (string-append "/user/" (uri-encode (ref params "nick"))
                      "/timeline/" next))
      (else #f))))

(define (next-link state)
  (let ((path (next-page-path state)))
    (if path
      `((p (@ (class "next")) (a (@ (href ,path)) "Next page")))
      '())))

(define (feed-grid state)
  (let* ((items (ref state "feed" "items"))
         (cards (if (vector? items)
                  (filter-map feed-card (vector->list items))
                  '())))
    (if (null? cards)
      '(p "No posts.")
      `(div (@ (class "feed")) ,@cards))))

;; The home page: iFunny's Featured feed.  STATE is #f when the feed could not
;; be loaded; the page still answers then.
(define (home-template state)
  `(html ,(html-head "Featured")
         (body
           ,heading
           ,@(if state
               `(,(channel-nav state)
                 (h1 "Featured")
                 ,(feed-grid state)
                 ,@(next-link state))
               '((nav (@ (class "channels"))
                      (a (@ (href "/")) "Featured"))
                 (h1 "Featured")
                 (p "The Featured feed could not be loaded from iFunny right "
                    "now. Post links keep working; try again in a minute.")))
           ,footer)))

;; A user's profile: avatar, nick, bio and counters, then their posts.
(define (user-template state)
  (let* ((user (ref state "user" "data"))
         (nick (ref user "nick"))
         (avatar (ref user "avatar" "s"))
         (about (ref user "about"))
         (subscribers (ref user "subscribers"))
         (featured (ref user "featured")))
    (unless (non-empty-string? nick)
      (throw 'user-missing))
    `(html ,(html-head nick)
           (body
             ,heading
             ,(channel-nav state)
             (div (@ (class "profile"))
                  ,@(if (non-empty-string? avatar)
                      `((img (@ (src ,(proxy avatar))
                                (alt "")
                                (width "100")
                                (height "100"))))
                      '())
                  (h1 ,nick))
             ,@(if (non-empty-string? about) `((p ,about)) '())
             ,@(let ((counters
                       (filter-map
                         (lambda (count label)
                           (and (integer? count)
                                (string-append (number->string count) " " label)))
                         (list subscribers featured)
                         '("subscribers" "featured"))))
                 (if (null? counters)
                   '()
                   `((p (@ (class "counters")) ,(string-join counters " · ")))))
             ,(feed-grid state)
             ,@(next-link state)
             ,footer))))

;; A channel, a tag or a further page of Featured.
(define (feed-template state)
  (let ((name (let ((name (ref state "feed" "name")))
                (if (non-empty-string? name) name "Featured"))))
    `(html ,(html-head name)
           (body
             ,heading
             ,(channel-nav state)
             (h1 ,name)
             ,(feed-grid state)
             ,@(next-link state)
             ,footer))))

;; STATE is the page's parsed __INITIAL_STATE__ (see (unfunny scraping)).
(define (post-template state)
  (let ((item (post-item state)))
    (if (video-post? item)
      (video-template item)
      (picture-template item))))

;; Below a post: who posted it and its tags, linking to their UNfunny pages.
(define (post-links item)
  (let ((nick (ref item "creator" "nick"))
        (tags (ref item "tags")))
    `(,@(if (non-empty-string? nick)
          `((p (@ (class "author"))
               "by " (a (@ (href ,(string-append "/user/" (uri-encode nick))))
                        ,nick)))
          '())
      ,@(let ((tags (if (vector? tags)
                      (filter non-empty-string? (vector->list tags))
                      '())))
          (if (null? tags)
            '()
            `((p (@ (class "tags"))
                 ,@(map (lambda (tag)
                          `(a (@ (href ,(string-append "/tags/"
                                                       (uri-encode tag))))
                              "#" ,tag))
                        tags))))))))

(define (picture-template item)
  (let ((title (post-title item)))
    `(html ,(html-head title)
           (body
             ,heading
             (h1 ,title)
             (img (@ (alt ,title)
                     (src ,(proxy (ref item "url")))))
             ,@(post-links item)
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
             ,@(post-links item)
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

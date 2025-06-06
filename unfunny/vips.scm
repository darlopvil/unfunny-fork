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

(define-module (unfunny vips)
  #:use-module (rnrs)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:export (init-vips
            vips-init
            vips-shutdown
            vips-image-new-from-buffer
            vips-image-new
            vips-extract-area
            vips-image-get-width
            vips-image-get-height
            vips-pngsave-buffer))

(define vips
  (dynamic-link
    (if (getenv "VIPS_PATH")
      (getenv "VIPS_PATH")
      "libvips")))

(define vips-init
  (pointer->procedure int
                      (dynamic-func "vips_init" vips)
                      (list '*)))
(define init-vips (lambda _ (vips-init (string->pointer "unfunny"))))

(define vips-shutdown
  (pointer->procedure void
                      (dynamic-func "vips_shutdown" vips)
                      (list)))

(define vips-image-new-from-buffer
  (pointer->procedure '*
                      (dynamic-func "vips_image_new_from_buffer" vips)
                      (list '* size_t '* '*)))

(define vips-image-new
  (pointer->procedure '*
                      (dynamic-func "vips_image_new" vips)
                      (list)))

(define vips-extract-area
  (pointer->procedure void
                      (dynamic-func "vips_extract_area" vips)
                      (list '* '* int int int int '*)))

(define vips-image-get-width
  (pointer->procedure int
                      (dynamic-func "vips_image_get_width" vips)
                      (list '*)))

(define vips-image-get-height
  (pointer->procedure int
                      (dynamic-func "vips_image_get_height" vips)
                      (list '*)))

(define vips-pngsave-buffer
  (pointer->procedure int
                      (dynamic-func "vips_pngsave_buffer" vips)
                      (list '* '* '* '*)))

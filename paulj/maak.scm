;;; dotfiles - paul's guix system configuration

;; Copyright (C) Paul Jewell <paul@teulu.org>

;; sss is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
 
;; sss is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sss.  If not, see <https://www.gnu.org/licenses/>.

(define-module (maak)
  #:declarative? #t
  #:use-module (maak maak))

(define (system-reconfigure)
  "Re-configure the Guix system,"
  ($ (list "sudo guix system reconfigure"
           "-L ~/.dotfiles"
           (format #f "~~/.dotfiles/paulj/systems/~a.scm" (gethostname)))))

(define (this-hostname)
  (format #t "systems/~a.scm" (gethostname)))

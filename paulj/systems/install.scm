;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 David Wilson <david@daviwil.com>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Generate a bootable image (e.g. for USB sticks, etc.) with:
;; $ guix system disk-image nongnu/system/install.scm

(define-module (paulj systems install)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  
  #:use-module (saayix packages text-editors)
  #:use-module (saayix services system rfkill)

  #:use-module (gnu packages emacs)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages file-systems)

  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:export (installation-os-nonfree))

(define %nonguix-key
  (plain-file "nonguix.pub"
              "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))

(define %channels
  (list (channel
          (name 'guix)
          (url "https://git.savannah.gnu.org/git/guix.git")
          (branch "master")
          (introduction
           (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
             "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
        
        (channel
         (name 'nonguix)
         (url "https://gitlab.com/nonguix/nonguix")
         (branch "master")
         (introduction
          (make-channel-introduction
           "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
           (openpgp-fingerprint
            "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
        
        (channel
         (name 'saayix)
         (url "https://codeberg.org/look/saayix.git")
         (branch "entropy")
         (introduction
          (make-channel-introduction
           "12540f593092e9a177eb8a974a57bb4892327752"
           (openpgp-fingerprint
            "3FFA 7335 973E 0A49 47FC  0A8C 38D5 96BE 07D3 34AB")))))
  )

(define installation-os-nonfree
  (operating-system
   (inherit installation-os)
   (kernel linux)
   (firmware (list linux-firmware))
   (initrd microcode-initrd)

   (packages
    (cons* exfat-utils
                                        ; fuse-exfat
           git
           curl
           stow
           helix
           emacs-no-x-toolkit
           (operating-system-packages installation-os)))
   (services
    (cons* (service rfkill-service-type)
           (modify-services (operating-system-user-services installation-os)
                            (guix-service-type
                             config => (guix-configuration
                                        (inherit config)
                                        (guix (guix-for-channels %channels))
                                        (authorize-key? #t)
                                        (authorized-keys
                                         (cons* %nonguix-key
                                                %default-authorized-guix-keys))
                                        (substitute-urls
                                         '("https://ci.guix.gnu.org"
                                           "https://substitutes.nonguix.org"
                                           "https://bordeaux.guix.gnu.org"))
                                        (channels %channels)
                                        (extra-options '("--max-jobs=6"
                                                         "--cores=0")))))))))

installation-os-nonfree

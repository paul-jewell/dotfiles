;; NOTE: This file is generated from ~/.dotfiles/System.org.  Please see commentary there.

(define-module (zeus)
  #:use-module (base-system)
  #:use-module (gnu))

(operating-system
 (inherit base-operating-system)
 (host-name "zeus")

 (mapped-devices
  (list (mapped-device
         (source (uuid "<<Need to change this...>>"))
         (target "system-root")
         (type luks-device-mapping))))

 (file-systems (cons*
                (file-system
                 (device (file-system-label "zeus"))
                 (mount-point "/")
                 (type "ext4")
                 (dependencies mapped-devices))
                (file-system
                 (device "/dev/nvme0n1p1")
                 (mount-point "/boot/efi")
                 (type "vfat"))
                %base-file-systems)))

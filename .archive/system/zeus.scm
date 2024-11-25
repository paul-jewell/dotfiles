;; -*- buffer-read-only: t; -*-
;; NOTE: This file is generated from ~/.dotfiles/System.org.  Please see commentary there.
;; Note - if you re-install, you need to review the uuid entries below.

(define-module (zeus)
  #:use-module (base-system)
  #:use-module (gnu))

(operating-system
  (inherit base-operating-system)
  (host-name "zeus")

  (swap-space
   (target (uuid "a0a103a5-cef2-446b-a2ff-8ffbee6890de")))
;  (swap-devices
;   (list (uuid "a0a103a5-cef2-446b-a2ff-8ffbee6890de")))
  (file-systems
   (cons* (file-system
            (mount-point "/boot/efi")
            (device (uuid "F21D-F4AF" 'fat32))
            (type "vfat"))
          (file-system
            (mount-point "/")
            (device
             (uuid "dbac54d4-4507-4205-bc72-8b1e7abc3c8f"
                   'btrfs))
            (type "btrfs"))
          %base-file-systems)))

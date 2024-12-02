(define-module (paulj systems claudio)
  #:use-module (paulj systems base)
  #:use-module (paulj systems common)
  #:use-module (paulj home-services video)
  #:use-module (paulj home-services finance)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services sound)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system uuid)
  #:use-module (gnu system file-systems)
  #:use-module (nongnu packages linux))

(system-config
 #:home
 (home-environment
  (services (cons* (service home-pipewire-service-type)
                   (service home-finance-service-type)
                   (service home-video-service-type)
                   common-home-services)))
 
 #:system
 (operating-system
  (host-name "claudio")

  (keyboard-layout (keyboard-layout "gb" "extd" #:model "thinkpad"))
  (bootloader (bootloader-configuration
               (bootloader grub-efi-bootloader)
               (targets (list "/boot/efi"))
               (keyboard-layout keyboard-layout)))

  (swap-devices (list (swap-space
                       (target (uuid
                                "" ; to be inserted
                                )))))

  (file-systems (cons* (file-system
                        (mount-point "/")
                        (device (uuid
                                 ""  ; to be inserted
                                 'btrfs))
                        (type "btrfs")) 
                       (file-system
                        (mount-point "/boot/efi")
                        (device (uuid "" ; To be inserted
                                      'fat32))
                        (type "vfat")) %base-file-systems))
  (services (list))))

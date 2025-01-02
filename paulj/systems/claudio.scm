(define-module (paulj systems claudio)
  #:use-module (paulj systems base)
  #:use-module (paulj systems common)
  #:use-module (paulj systems claudio file-systems)
  
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
  #:use-module (gnu system)

  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))

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

  ;; Use non-free Linux and firmware
  (kernel linux)
  (firmware linux-firmware)
  (initrd microcode-initrd)
  
  (keyboard-layout (keyboard-layout "gb" "extd" #:model "thinkpad"))

  (bootloader (bootloader-configuration
               (bootloader grub-efi-bootloader)
               (targets '("/boot/efi"))
               (keyboard-layout keyboard-layout)))
  
  (swap-devices (list (swap-space
                       (target /swap))))

  (file-systems %ext4-file-systems)
  (services (list))))

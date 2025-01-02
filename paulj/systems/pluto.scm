(define-module (paulj systems pluto)
  #:use-module (paulj systems base)
  #:use-module (paulj systems common)
  #:use-module (paulj systems pluto file-systems)
  #:use-module (paulj home-services video)
  #:use-module (paulj home-services finance)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services sound)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system uuid)
  #:use-module (gnu system))

(system-config
 #:home
 (home-environment
  (services (cons* (service home-pipewire-service-type)
                   (service home-finance-service-type)
                   (service home-video-service-type)
                   common-home-services)))
 
 #:system
 (operating-system
  (host-name "pluto")

  (keyboard-layout (keyboard-layout "gb" "extd"))
  
  (bootloader (bootloader-configuration
               (bootloader grub-efi-bootloader)
               (targets (list "/boot/efi"))
               (keyboard-layout keyboard-layout)))

  (swap-devices (list (swap-space
                       (target (uuid
                                "755a3ec1-9552-41ea-b1ab-9a9298aab04a")))))

  (file-systems %pluto-file-systems)
  (services (list))))

(define-module (paulj systems neptune)
  ;; System configuration
  #:use-module (paulj systems base)
  #:use-module (paulj systems neptune file-systems)
  ;; Home services
  #:use-module (paulj home-services x390)
  #:use-module (paulj home-services common)
  #:use-module (paulj home-services desktop)
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
  (services (cons*
             service home-x390
             (service home-desktop-service-type) ;; Graphical desktop service
             (service home-pipewire-service-type)
             (service home-finance-service-type)
;;             (service home-video-service-type)
             common-home-services)))
 
 #:system
 (operating-system
  (host-name "neptune")

  ;; Use non-free Linux and firmware
  (kernel linux)
  (firmware (list linux-firmware))
  (initrd microcode-initrd)
  
  (keyboard-layout (keyboard-layout "gb" "extd" #:model "thinkpad"))

  (bootloader (bootloader-configuration
               (bootloader grub-efi-bootloader)
               (targets '("/boot/efi"))
               (keyboard-layout keyboard-layout)))
  
  (swap-devices
   (list
    (swap-space (target (file-system-label "guix-swap")))))

  (file-systems %btrfs-file-systems)
  (services (list))))

(define-module (paulj systems pluto)
  #:use-module (paulj systems base)
  #:use-module (gnu))

(use-service-modules cups desktop networking ssh xorg)

(system-config
 #:system
 (operating-system
  (host-name "pluto")

  (bootloader (bootloader-configuration
               (bootloader grub-efi-bootloader)
               (targets (list "/boot/efi"))
               (keyboard-layout keyboard-layout)))

  (swap-devices (list (swap-space
                       (target (uuid
                                "755a3ec1-9552-41ea-b1ab-9a9298aab04a")))))

  (file-systems (cons* (file-system
                        (mount-point "/")
                        (device (uuid
                                 "a5e463ab-8489-42e2-bf20-c3fad9423ea7"
                                 'xfs))
                        (type "xfs"))
                       (file-system
                        (mount-point "/boot/efi")
                        (device (uuid "7557-8745"
                                      'fat32))
                        (type "vfat")) %base-file-systems))
  (services (list))))

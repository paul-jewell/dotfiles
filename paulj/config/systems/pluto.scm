(define-module (paulj systems pluto)
  (use-module (gnu)))

(use-service-modules cups desktop networking ssh xorg)

(system-config
 #:system
 (operating-system
  (locale "en_GB.utf8")
  (timezone "Europe/London")
  (keyboard-layout (keyboard-layout "gb" "extd"))
  (host-name "pluto")

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                 (name "paul")
                 (comment "Paul Jewell")
                 (group "users")
                 (home-directory "/home/paul")
                 (supplementary-groups '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

  ;; Packages installed system-wide.  Users can also install packages
  ;; under their own account: use 'guix search KEYWORD' to search
  ;; for packages and 'guix install PACKAGE' to install a package.
  (packages (append (list (specification->package "nss-certs"))
                    %base-packages))

  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
   (append (list (service xfce-desktop-service-type)

                 ;; To configure OpenSSH, pass an 'openssh-configuration'
                 ;; record as a second argument to 'service' below.
                 (service openssh-service-type)
                 (service cups-service-type)
                 (set-xorg-configuration
                  (xorg-configuration (keyboard-layout keyboard-layout))))

           ;; This is the default list of services we
           ;; are appending to.
           %desktop-services))
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
                        (type "vfat")) %base-file-systems))))

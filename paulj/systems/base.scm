(define-module (paulj systems base)
  #:use-module (srfi srfi-1)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system nss)
  #:export (system-config))

(use-service-modules guix admin sysctl pm avahi dbus cups desktop linux
                     mcron networking xorg ssh audio virtualization)

(use-package-modules audio video nfs certs shells ssh linux bash emacs gnome
                     networking wm fonts libusb cups freedesktop file-systems
                     version-control package-management sync vim)

(define-public base-operating-system
  (operating-system
   (host-name "base-system")
   (timezone "Europe/London")
   (locale "en_GB.utf8")
   (keyboard-layout (keyboard-layout "gb" "extd"))
   
   ;; Use UEFI version of GRUB with EFI System
   ;; Partition mounted on /boot/efi 
   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))

   (file-systems (cons*
                  (file-system
                   (mount-point "/tmp")
                   (device "none")
                   (type "tmpfs")
                   (check? #f))
                  %base-file-systems))

   (users (cons* (user-account
                  (name "paul")
                  (comment "Paul Jewell")
                  (group "users")
                  (home-directory "/home/paul")
                  (supplementary-groups '("wheel" "netdev" "lp" "audio" "video")))
                 %base-user-accounts))

   (packages (cons* emacs-no-x-toolkit
                    exfat-utils
                    fuse-exfat
                    git
                    gvfs
                    stow
                    vim
                    nextcloud-client
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
   ;; Allow resolution of '.local' host names with mDNS
   (name-service-switch %mdns-host-lookup-nss)))

(define* (system-config #:key system home)
  (operating-system
   (inherit system)
   (timezone "Europe/London")
   (locale "en_GB.utf8")

   (keyboard-layout (keyboard-layout "gb" "extd"))

   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets '("/boot/efi"))
                (keyboard-layout keyboard-layout)))

   (users (cons* (user-account
                  (name "paul")
                  (comment "Paul Jewell")
                  (group "users")
                  (home-directory "/home/paul")
                  (supplementary-groups '("wheel" "netdev" "lp" "audio" "video")))
                 %base-user-accounts))

   (packages (cons* emacs-no-x-toolkit
                    exfat-utils
                    fuse-exfat
                    git
                    gvfs
                    stow
                    vim
                    %base-packages))

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

   ;; Allow resolution of '.local' host names with mDNS
   (name-service-switch %mdns-host-lookup-nss)))

(define-module (paulj systems base)
  #:use-module (srfi srfi-1)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system nss)
  #:use-module (gnu system setuid)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu packages video)
  #:use-module (nongnu system linux-initrd)
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

   ;; Use non-free Linux and firmware
   (kernel linux)
   (firmware (list kinux-firmware))
   (initrd microcode-initrd)

   ;; Additional kernel modules
   (kernel-loadable-modules (list v4l2loopback-linux-module))
   
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
   
   (users (cons (user-account
                 (name "paul")
                 (comment "Paul Jewell")
                 (group "users")
                 (home-directory "/home/paul")
                 (supplementary-groups '("wheel" "netdev" "lp" "audio" "video")))
                %base-user-accounts))
   
   (groups (cons (user-group (system? #t) (name "realtime"))
           %base-groups))
                                                                                
   (packages (cons* bluez
                    bluez-alsa
                    brightnessctl
                    emacs-no-x-toolkit
                    exfat-utils
                    fuse-exfat
                    git
                    gvfs
                    intel-media-driver/nonfree
                    libva-utils
                    stow
                    vim
                    nextcloud-client
                    %base-packages))
                                       
   ;; Below is the list of system services.  To search for available
   ;; services, run 'guix system search KEYWORD' in a terminal.

   (services (append
              (modify-services %base-services
                               (delete login-service-type)
                               (delete mingetty-service-type)
                               (delete console-font-service-type))
              (list
               ;; seat management (can't use seatd because wireplumber depends on elogind)
               (service elogind-service-type)
               
               ;; Configure TTYs and graphical greeter
               (service console-font-service-type
                        (map (lambda (tty)
                               ;; use a larger font for HIDPI screens
                               (cons tty (file-append
                                          font-terminus
                                          "/share/consolefonts/ter-132n")))
                             '("tty1" "tty2" "tty3")))
               
               (service greetd-service-type
                        (greetd-configuration
                         (greeter-supplementary-groups (list "video" "input"))
                         (terminals
                          (list
                           (greetd-terminal-configuration
                            (terminal-vt "1")
                            (terminal-switch #t))
                           (greetd-terminal-configuration (terminal-vt "2"))
                           (greetd-terminal-configuration (terminal-vt "3"))))))
               
               (service screen-locker-service-type
                        (screen-locker-configuration
                         (name "swaylock")
                         (program (file-append swaylock "/bin/swaylock"))
                         (using-pam? #t)
                         (using-setuid? #f)))
               
               ;; Set up Polkit to `allow' wheel users to run admin tasks
               polkit-wheel-service
               
               ;; Give certain programs super-user access
               ;; (simple-service 'mount-setuid-helpers
               ;;                 setuid-program-service-type
               ;;                 (map (lambda (program)
               ;;                        (setuid-program
               ;;                         (program program)))
               ;;                      (list (file-append nfs-utils "/sbin/mount.nfs")
               ;;                            (file-append ntfs-3g "/sbin/mount.ntfs-3g"))))

               ;; Networking services
               (service network-manager-service-type
                        (network-manager-configuration
                         (vpn-plugins
                          (list network-manager-openvpn))))
               (service wpa-supplicant-service-type) ;; Needed by NetworkManager
               (service modem-manager-service-type)  ;; For cellular modems
               (service bluetooth-service-type
                        (bluetooth-configuration
                         (auto-enable? #t)))
               (service usb-modeswitch-service-type)
            
               ;; Basic desktop system services
               (service avahi-service-type)
               (service udisks-service-type)
               (service cups-pk-helper-service-type)
               (service geoclue-service-type)
               (service polkit-service-type)
               (service dbus-root-service-type)
               fontconfig-file-system-service ;; Manage the font config cache
               
               ;; Power and thermal management services
               
               (service thermald-service-type)
               
               ;; Enable JACK to enter realtime mode
               
               (service pam-limits-service-type
                        (list
                         (pam-limits-entry "@realtime" 'both 'rtprio 99)
                         (pam-limits-entry "@realtime" 'both 'nice -19)
                         (pam-limits-entry "@realtime" 'both 'memlock 'unlimited)))
               
               ;; Enable SSH access
               (service openssh-service-type
                        (openssh-configuration
                         (openssh openssh-sans-x)
                         (port-number 2222)))
               
               ;; Enable printing and scanning
               (service sane-service-type)
               (service cups-service-type
                        (cups-configuration
                         (web-interface? #t)
                         (extensions
                          (list cups-filters))))
               
               ;; Set up the X11 socket directory for XWayland
               (service x11-socket-directory-service-type)
               
               ;; Sync system clock with time servers
               (service ntp-service-type)
               
               ;; Add udev rules for MTP (mobile) devices for non-root user access
               (simple-service 'mtp udev-service-type (list libmtp))
               
               ;; Add udev rules for a few packages
               (udev-rules-service 'pipewire-add-udev-rules pipewire)
               (udev-rules-service 'brightnessctl-udev-rules brightnessctl)
               
               ;; Schedule cron jobs for system tasks
               (simple-service 'system-cron-jobs
                               mcron-service-type
                               (list
                                ;; Run `guix gc' 5 minutes after midnight every day.
                                ;; Clean up generations older than 2 months and free
                                ;; at least 10G of space.
                                #~(job "5 0 * * *" "guix gc -d 2m -F 10G"))))))
   
   ;; Allow resolution of '.local' host names with mDNS
   
   (name-service-switch %mdns-host-lookup-nss)))


;; I believe base-configuration (above) is no longer in use...



(define* (system-config #:key system home)
  (operating-system
   (inherit system)
   (timezone "Europe/London")
   (locale "en_GB.utf8")

   (keyboard-layout (keyboard-layout "gb" "extd"))

   (kernel linux)
   (firmware (or (operating-system-firmware system)
                 (list linux-firmware)))

   (initrd microcode-initrd)

   ;; Additional kernel modules
   (kernel-loadable-modules (list v4l2loopback-linux-module))
   
   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets '("/boot/efi"))
                (keyboard-layout keyboard-layout)))

   (users (cons (user-account
                 (name "paul")
                 (comment "Paul Jewell")
                 (group "users")
                 (home-directory "/home/paul")
                 (supplementary-groups '("wheel" "netdev" "lp" "audio" "video" "realtime")))
                %base-user-accounts))

   (groups (cons (user-group (system? #t) (name "realtime"))
                 %base-groups))

   
   (packages (cons* emacs
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
    (append
     (modify-services %base-services
                      (delete login-service-type)
                      (delete mingetty-service-type)
                      (delete console-font-service-type))
     (operating-system-user-services system)
     (list
      ;; Set up my home configuration
      (service guix-home-service-type
               `(("paul" ,home)))
      
      ;; seat management (can't use seatd because wireplumber depends on elogind)
      (service elogind-service-type)
      
      ;; Configure TTYs and graphical greeter
      (service console-font-service-type
               (map (lambda (tty)
                      ;; use a larger font for HIDPI screens
                      (cons tty (file-append
                                 font-terminus
                                 "/share/consolefonts/ter-120n")))
                    '("tty1" "tty2" "tty3")))
      (service greetd-service-type
               (greetd-configuration
                (greeter-supplementary-groups (list "video" "input"))
                (terminals
                 (list
                  (greetd-terminal-configuration
                   (terminal-vt "1")
                   (terminal-switch #t))
                  (greetd-terminal-configuration (terminal-vt "2"))
                  (greetd-terminal-configuration (terminal-vt "3"))))))
      
      (service screen-locker-service-type
               (screen-locker-configuration
                (name "swaylock")
                (program (file-append swaylock "/bin/swaylock"))
                (using-pam? #t)
                (using-setuid? #f)))
      
      ;; Set up Polkit to `allow' wheel users to run admin tasks
      polkit-wheel-service
      
      ;; Give certain programs super-user access
      ;; (simple-service 'mount-setuid-helpers
      ;;                 setuid-program-service-type
      ;;                 (map (lambda (program)
      ;;                        (setuid-program
      ;;                         (program program)))
      ;;                      (list (file-append nfs-utils "/sbin/mount.nfs")
      ;;                            (file-append ntfs-3g "/sbin/mount.ntfs-3g"))))


      ;; Networking services
      (service network-manager-service-type
               (network-manager-configuration
                (vpn-plugins
                 (list network-manager-openvpn))))
      (service wpa-supplicant-service-type) ;; Needed by NetworkManager
      (service modem-manager-service-type)  ;; For cellular modems
      (service bluetooth-service-type
               (bluetooth-configuration
                (auto-enable? #t)))
      (service usb-modeswitch-service-type)
      
      ;; Basic desktop system services
      (service avahi-service-type)
      (service udisks-service-type)
      (service cups-pk-helper-service-type)
      (service polkit-service-type)
      (service dbus-root-service-type)
      fontconfig-file-system-service ;; Manage the font config cache
      
      ;; Power and thermal management services
      (service thermald-service-type)
      (service tlp-service-type
                        (tlp-configuration
                         (cpu-boost-on-ac? #t)
                         (wifi-pwr-on-bat? #t)))

      ;; Enable JACK to enter realtime mode
      (service pam-limits-service-type
               (list
                (pam-limits-entry "@realtime" 'both 'rtprio 99)
                (pam-limits-entry "@realtime" 'both 'nice -19)
                (pam-limits-entry "@realtime" 'both 'memlock 'unlimited)))
      
      ;; Enable SSH access
      (service openssh-service-type
               (openssh-configuration
                (openssh openssh-sans-x)
                (port-number 2222)))
      
      ;; Enable printing and scanning
      (service sane-service-type)
      (service cups-service-type
               (cups-configuration
                (web-interface? #t)
                (extensions
                 (list cups-filters))))
      
      ;; Set up the X11 socket directory for XWayland
      (service x11-socket-directory-service-type)
      
      ;; Sync system clock with time servers
      (service ntp-service-type)
      
      ;; Add udev rules for MTP (mobile) devices for non-root user access
      (simple-service 'mtp udev-service-type (list libmtp))
      
      ;; Add udev rules for a few packages
      (udev-rules-service 'pipewire-add-udev-rules pipewire)
      (udev-rules-service 'brightnessctl-udev-rules brightnessctl)
      ;; Schedule cron jobs for system tasks
      (simple-service 'system-cron-jobs
                      mcron-service-type
                      (list
                       ;; Run `guix gc' 5 minutes after midnight every day.
                       ;; Clean up generations older than 2 months and free
                       ;; at least 10G of space.
                       #~(job "5 0 * * *" "guix gc -d 2m -F 10G"))))))
   
   ;; Allow resolution of '.local' host names with mDNS    
   (name-service-switch %mdns-host-lookup-nss)))

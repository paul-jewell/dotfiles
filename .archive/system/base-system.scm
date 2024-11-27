;; -*- buffer-read-only: t; -*-
;; NOTE: This file is generated from ~/.dotfiles/System.org.  Please see commentary there.

(define-module (base-system)
  #:use-module (gnu)
  #:use-module (srfi srfi-1)
  #:use-module (gnu system nss)
  #:use-module (gnu services pm)
  #:use-module (gnu services cups)
  #:use-module (gnu services desktop)
  ;;      #:use-module (gnu services docker)
  #:use-module (gnu services networking)
  ;;  #:use-module (gnu services virtualization)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages package-management)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (paulj packages paulj-dwm)
  #:use-module (paulj packages paulj-st)
  #:use-module (paulj packages paulj-dmenu)
  #:use-module (paulj packages paulj-slock))

(use-service-modules nix)
(use-service-modules desktop xorg)
(use-service-modules ssh)
(use-package-modules certs)
(use-package-modules shells)

;; Allow members of the "video" group to change the screen brightness.
(define %backlight-udev-rule
  (udev-rule
   "90-backlight.rules"
   (string-append "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "KERNEL==\"intel_backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\""
                  "\n"
                  "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "KERNEL==\"intel_backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))

(define %touchpad-udev-rule
  (udev-rule
   "10-trackpoint.rules"
   (string-append "ACTION==\"add\", SUBSYSTEM==\"input\", "
                  "ATTR{name}==\"TPPS/2 IBM Trackpoint\", "
                  "ATTR{device/sensitivity}=\"240\", "
                  "ATTR{device/press_to_select}=\"1\"")))

(define %my-desktop-services
  (modify-services %desktop-services
                   (elogind-service-type config =>
                                         (elogind-configuration (inherit config)
                                                                (handle-lid-switch-external-power 'suspend)))
                   (udev-service-type config =>
                                      (udev-configuration (inherit config)
                                                          (rules (cons* %backlight-udev-rule
                                                                        %touchpad-udev-rule
                                                                        (udev-configuration-rules config)))))
                   (network-manager-service-type config =>
                                                 (network-manager-configuration (inherit config)
                                                                                (vpn-plugins
                                                                                 (list network-manager-openvpn))))))

(define %xorg-libinput-config
  "Section \"InputClass\"
     Identifier \"Touchpads\"
     Driver \"libinput\"
     MatchDevicePath \"/dev/input/event*\"
     MatchIsTouchpad \"on\"

     Option \"Tapping\" \"on\"
     Option \"TappingDrag\" \"on\"
     Option \"DisableWhileTyping\" \"on\"
     Option \"MiddleEmulation\" \"on\"
     Option \"ScrollMethod\" \"twofinger\"
   EndSection
   Section \"InputClass\"
     Identifier \"Keyboards\"
     Driver \"libinput\"
     MatchDevicePath \"/dev/input/event*\"
     MatchIsKeyboard \"on\"
   EndSection")

(define-public base-operating-system
  (operating-system
    (host-name "base")
    (timezone "Europe/London")
    (locale "en_GB.utf8")
    
    ;; Use non-free Linux and firmware
    (kernel linux)
    (firmware (list linux-firmware))
    (initrd microcode-initrd)
    
    ;; Choose UK English keyboard layout, with the extd layout.
    (keyboard-layout (keyboard-layout "gb" "extd"
                                      #:model "thinkpad"
                                      #:options '("ctrl:nocaps")))
    
    ;; Use the UEFI variant of GRUB with the EFI System
    ;; Partition mounted on /boot/efi.
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets '("/boot/efi"))
                 (keyboard-layout keyboard-layout)))
    
    
    ;; Guix doesn't like it when there isn't a file-systems
    ;; entry, so add one that is meant to be overridden
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
                  (supplementary-groups '(
                                          "wheel"     ;; sudo
                                          "netdev"    ;; network devices
                                          "kvm"
                                          "tty"
                                          "input"
                                          ;;                                            "docker"
                                          "realtime"  ;; Enable realtime scheduling
                                          "lp"        ;; control bluetooth devices
                                          "audio"     ;; control audio devices
                                          "video")))  ;; control video devices
                 %base-user-accounts))
    
    ;; Add the 'realtime' group
    (groups (cons (user-group (system? #t) (name "realtime"))
                  %base-groups))
    
    ;; Install bare-minimum system packages
    (packages (append (list
                       git
                       ntfs-3g
                       exfat-utils
                       fuse-exfat
                       stow
                       neovim
                       ;;                          emacs
                       bluez
                       bluez-alsa
                       pulseaudio
                       tlp
                       xf86-input-libinput
                       nss-certs     ;; for HTTPS access
                       gvfs)         ;; for user mounts
                      %base-packages))
    
    ;; Use the "desktop" services, which include the X11 log-in service,
    ;; networking with NetworkManager, and more
    (services (cons* (service slim-service-type
                              (slim-configuration
                               (xorg-configuration
                                (xorg-configuration
                                 (keyboard-layout keyboard-layout)
                                 (extra-config (list %xorg-libinput-config))))))
                     ;;                     (service xfce-desktop-service-type)
                     (service openssh-service-type)
                     (service tlp-service-type
                              (tlp-configuration
                               (cpu-boost-on-ac? #t)
                               (wifi-pwr-on-bat? #t)))
                     (pam-limits-service ;; This enables JACK to enter realtime mode
                      (list
                       (pam-limits-entry "@realtime" 'both 'rtprio 99)
                       (pam-limits-entry "@realtime" 'both 'memlock 'unlimited)))
                     (service thermald-service-type)
                     ;;                      (service docker-service-type)
                     (service cups-service-type
                              (cups-configuration
                               (web-interface? #t)
                               (extensions
                                (list cups-filters))))
                     (bluetooth-service #:auto-enable? #t)
                     (remove (lambda (service)
                               (eq? (service-kind service) gdm-service-type))
                             %my-desktop-services)))
    
    ;; Allow resolution of '.local' host names with mDNS
    (name-service-switch %mdns-host-lookup-nss)))

(define-module (paulj home-services desktop)
  #:use-module (paulj packages fonts)
  #:use-module (gnu)
  #:use-module (gnu home services)
  #:use-module (gnu packages)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (nongnu packages mozilla)
  #:use-module (saayix packages text-editors)
  #:use-module (saayix packages fonts)
  #:export (home-desktop-service-type))

(use-package-modules compression curl fonts freedesktop gimp glib gnome gnome-xyz
                     gstreamer kde-frameworks linux music package-management
                     password-utils pdf pulseaudio shellutils ssh syncthing terminals
                     video web-browsers wget wm xdisorg xorg)

(define (home-desktop-profile-service config)
  (list sway
        waybar
        swayidle
        swaylock
        fuzzel
        wl-clipboard
        mako
        foot
        firefox
        alacritty
        helix
        gammastep
        grimshot ;; grimshot --notify copy area
        network-manager-applet

        ;; Compatibility for older Xorg applications
        xorg-server-xwayland

        ;; Flatpak and XDG utilities
        xdg-desktop-portal-gtk
        xdg-desktop-portal-wlr
        xdg-utils ;; For xdg-open, etc
        xdg-dbus-proxy
        shared-mime-info
        (list glib "bin")

        ;; Appearance
        matcha-theme
        papirus-icon-theme
        breeze-icons ;; For KDE apps
        gnome-themes-extra
        adwaita-icon-theme
        hicolor-icon-theme
        yaru-theme

        ;; Fonts
        font-jost
        font-iosevka-ss08
        font-iosevka-aile
        font-microsoft-cascadia
        font-jetbrains-mono
        font-google-noto
        font-google-noto-emoji
        font-liberation
        font-awesome
        font-nerd-fira-code
        font-ibm-plex
        
        ;; Browsers
        (specification->package "qtwayland@5")
        ;;qutebrowser
        ;;vimb
        nyxt

        ;; Authentication
        password-store

        gnome-keyring

        ;; Audio devices and media playback
        mpv
        mpv-mpris
        youtube-dl
        playerctl
        gstreamer
        gst-plugins-base
        gst-plugins-good
        gst-plugins-bad
        gst-plugins-ugly
        gst-libav
        alsa-utils
        pavucontrol

        ;; Graphics
        gimp-next

        ;; PDF reader
        zathura
        zathura-pdf-mupdf

        ;; File syncing
        syncthing-gtk

        ;; General utilities
        curl
        wget
        openssh
        zip
        unzip))

(define (home-desktop-environment-variables config)
  '(("_JAVA_AWT_WM_NONREPARENTING" . "1")))

(define home-desktop-service-type
  (service-type (name 'home-desktop)
                (description "My desktop environment service.")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-desktop-profile-service)
                       (service-extension
                        home-environment-variables-service-type
                        home-desktop-environment-variables)))
                (default-value #f)))

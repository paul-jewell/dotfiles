(define-module (paulj home-services common)
  #:use-module (paulj home-services emacs)
  #:use-module (paulj home-services desktop)
  #:use-module (paulj home-services udiskie)
  #:use-module (paulj home-services cli)
  
  #:use-module (gnu services)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu home services)
  #:use-module (gnu home services pm)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services syncthing)
  #:use-module (guix gexp))

(define-public common-home-services
  (list
   ;; Set environment variables for every session
   (simple-service 'profile-env-vars-service
                   home-environment-variables-service-type
                   '( ;; sort hidden (dot) files first in `ls` listings
                     ("LC_COLLATE" . "C")

                     ;; emacs is our editor
                     ("VISUAL" . "emacsclient")
                     ("EDITOR" . "emacsclient")

                     ;; Add some local directories to $PATH
                     ("PATH" . "$HOME/.bin:$HOME/.cargo/bin:$PATH")

                     ;; Set Wayand-specific environment variables (taken from RDE)
                     ("XDG_CURRENT_DESKTOP" . "sway")
                     ("XDG_SESSION_TYPE" . "wayland")
                     ("RTC_USE_PIPEWIRE" . "true")
                     ("SDL_VIDEODRIVER" . "wayland")
                     ("MOZ_ENABLE_WAYLAND" . "1")
                     ("CLUTTER_BACKEND" . "wayland")
                     ("ELM_ENGINE" . "wayland_egl")
                     ("ECORE_EVAS_ENGINE" . "wayland-egl")
                     ("QT_QPA_PLATFORM" . "wayland-egl")))

   ;; Set up the shell environment
   (service home-bash-service-type
            (home-bash-configuration
             (bash-profile
              `(,(plain-file "bash-sway-login"
                             (string-append
                              "if [ -z \"WAYLAND_DISPLAY\" ] && [ \"$XDG_VTNR\" -eq 1 ]; then\n"
                              "  exec sway\n"
                              "fi\n"))))
             (bashrc
              `(,(local-file "../files/bash-prompt")))))

   ;; Place other files
   (simple-service 'profile-files-service
                   home-files-service-type
                   (list `(".inputrc" ,(local-file "../files/inputrc"))))
      ;; GnuPG configuration
   (service home-gpg-agent-service-type
            (home-gpg-agent-configuration
             (pinentry-program
              (file-append pinentry-emacs "/bin/pinentry-emacs"))
             (ssh-support? #t)
             (default-cache-ttl 28800)
             (max-cache-ttl 28800)
             (default-cache-ttl-ssh 28800)
             (max-cache-ttl-ssh 28800)))

   ;; Emacs configuration
   (service home-emacs-config-service-type)

   ;; Run user dbus session
   (service home-dbus-service-type)


   ;; Start background jobs
   (service home-mcron-service-type
            (home-mcron-configuration
             (jobs
              (list
               #~(job
                  '(next-hour (range 0 24 4))
                  "~/.dotfiles/.bin/sync-passwords")))))

   ;; cli applications
   (service home-cli-service-type)
   
   ;; File synchronization
   (service home-syncthing-service-type)
   
   ;; Udiskie for auto-mounting devices
   (service home-udiskie-service-type)))
  
  

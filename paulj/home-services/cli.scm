;; cli.scm - Packages and services for all systems

(define-module (paulj home-services cli)
  #:use-module (gnu home services)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages build-tools)
  #:use-module (saayix packages file-managers)
  #:export (home-cli-service-type))

(define (home-cli-profile-service config)
  (list ripgrep
        khal
        yazi
        bat
        gcc-toolchain
        maak))

(define home-cli-service-type
  (service-type (name 'home-cli)
                (description "cli apps for all systems")
                (extensions
                 (list (service-extension
                        home-profile-service-type home-cli-profile-service)))
                (default-value #f)))


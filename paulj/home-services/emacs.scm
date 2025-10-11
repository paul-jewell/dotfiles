(define-module (paulj home-services emacs)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages finance)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix transformations)

  #:export (home-emacs-config-service-type))

(define (home-emacs-config-profile-service config)
  (list
   emacs-all-the-icons-dired
   emacs-async
   emacs-bbdb
   emacs-cape
   emacs-consult-dir
   emacs-corfu
   emacs-dashboard
   emacs-doom-modeline
   emacs-ef-themes
   emacs-elisp-slime-nav
   emacs-fontaine
   emacs-haskell-mode
   emacs-ledger-mode
   emacs-lua-mode
   emacs-marginalia
   emacs-markdown-mode
   emacs-multiple-cursors
   emacs-nix-mode
   emacs-no-littering
   emacs-orderless
   ;;emacs-quelpa-use-package
   emacs-rust-mode
   emacs-treemacs
   emacs-vertico
   emacs-web-mode
   emacs-which-key
   emacs-yaml-mode
   emacs-yasnippet-snippets
   
   ;; Dependencies for the above packages
   emacs-all-the-icons
   emacs-auto-complete
   emacs-avy
   emacs-cfrs
   emacs-cider
   emacs-clojure-mode
;;   emacs-compat
   emacs-consult
;;   emacs-dash
   emacs-embark
;;   emacs-f
   emacs-ht
   emacs-hydra
   ;; emacs-ly
   emacs-macrostep
   emacs-magit
   emacs-nerd-icons
   emacs-parseclj
   emacs-parseedn
   emacs-pfuture
   emacs-popup
   emacs-posframe
   ;; emacs-quelpa
   emacs-queue
;;   emacs-s
   emacs-sesman
   emacs-shrink-path
   emacs-slime
   emacs-spinner
   emacs-transient
   emacs-yasnippet
   
   ;; Support programs
   hunspell
   hunspell-dict-en-gb
   ))

(define home-emacs-config-service-type
  (service-type (name 'home-emacs-config)
                (description "Applies my personal Emacs configuation")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-emacs-config-profile-service)))
                (default-value #f)))


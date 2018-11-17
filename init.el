;;; init.el --- Startup file for emacs

;;; Commentary:
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;;; Code:

(package-initialize)

;; (defvar init-dir) ;; Initial directory for emacs configuration
;; (setq init-dir (file-name-directory (or load-file-name (buffer-file-name))))

;; Save emacs customisations in this file:
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)


;; Removed: 17/11 - Originally used to provide a local org-mode installation
;;                  Since the variable was not bound it was not being run

;; (if (boundp 'org-mode-user-lisp-path)
;;     (add-to-list 'load-path org-mode-user-lisp-path)
;;   (add-to-list 'load-path (expand-file-name "~/git/org-mode/lisp")))
 

;; (add-to-list 'auto-mode-alist '("\\.\\(org\\|org-archive\\|txt\\)$" . org-mode))
;;(require 'org-install)
;;;(require 'org)

(org-babel-load-file
  (expand-file-name "emacs-init.org"
                   user-emacs-directory))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button))))))
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ansi-color-names-vector
;;    ["#32302F" "#FB4934" "#B8BB26" "#FABD2F" "#83A598" "#D3869B" "#17CCD5" "#EBDBB2"])
;;  '(coffee-tab-width 2)
;;  '(custom-safe-themes
;;    (quote
;;     ("3fa81193ab414a4d54cde427c2662337c2cab5dd4eb17ffff0d90bca97581eb6" "dcb9fd142d390bb289fee1d1bb49cb67ab7422cd46baddf11f5c9b7ff756f64c" "eecacf3fb8efc90e6f7478f6143fd168342bbfa261654a754c7d47761cec07c8" "a4d03266add9a1c8f12b5309612cbbf96e1291773c7bc4fb685bfdaf83b721c6" "7366916327c60fdf17b53b4ac7f565866c38e1b4a27345fe7facbf16b7a4e9e8" "10e231624707d46f7b2059cc9280c332f7c7a530ebc17dba7e506df34c5332c4" "c9321e2db48a21fc656a907e97ee85d8cd86967855bf0bed3998bcf9195c758b" "2cce0dc83ab7109f59df81a6c4d66347d776813e55d6fbb4798ba088ab968d99" "b9b1a8d2ec1d5c17700e1a09256f33c2520b26f49980ed9e217e444c381279a9" "d320493111089afba1563bc3962d8ea1117dd2b3abb189aeebdc8c51b5517ddb" default)))
;;  '(global-ede-mode t)
;;  '(package-selected-packages
;;    (quote
;;     (cider-hydra smex go-mode org multiple-cursors slime jedi clj-refactor cider rainbow-delimiters highlight-parentheses paredit-everywhere paredit org-gcal eyebrowse projectile powerline all-the-icons-dired challenger-deep-theme all-the-icons hydra git-gutter magit flycheck dracula-theme counsel council swiper irony-eldoc company company-arduino company-c-headers company-ghc company-ghci company-irony company-irony-c-headers company-jedi company-lua company-math company-qml company-web irony ## seoul256-theme private-diary which-key use-package suscolors-theme scad-preview org-bullets ledger-mode gruvbox-theme go-autocomplete darktooth-theme bbdb)))
;;  '(show-paren-mode t))

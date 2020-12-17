;;; init.el --- Startup file for emacs

;;; Commentary:
;; Initial configuration file loaded by Emacs.
;;  - Sets the custom configuration file location
;; - loads the code in emacs-init.org
;; (c) 2017 - 2020 Paul Jewell
;; Licence: BSD

;;; Code:

;; Load machine local definitions
(load-file "~/.emacs.d/site-local.el")

(defvar *packages-initialised* nil)

(defun initialise-packages ()
  "Ensure `package-initialize' is called only once."
  (unless *packages-initialised*
    (package-initialize)
    (setq *packages-initialised* t)))

(initialise-packages)

;; To load external version of org-mode, clone the code from git:
;; > cd <directory below which you want the org code>
;; > git clone https://code.orgmode.org/bzg/org-mode.git
;; > cd org-mode
;; > make autoloads # creates org-loaddefs.el in the lisp directory

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(use-package org
    :ensure t)

(defvar init-dir) ;; Initial directory for emacs configuration
(setq init-dir (file-name-directory (or load-file-name (buffer-file-name))))

;; Save emacs customisations in this file:
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

(org-babel-load-file
 (expand-file-name "emacs-init.org"
		   user-emacs-directory))

;;; init.el ends here


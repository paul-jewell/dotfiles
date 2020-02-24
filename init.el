;;; init.el --- Startup file for emacs

;;; Commentary:
;; Initial configuration file loaded by Emacs.
;;  - Sets the custom configuration file location
;; - loads the code in emacs-init.org
;; (c) 2017 - 2020 Paul Jewell
;; Licence: BSD

;;; Code:

;(eval-after-load "org"
;  '(debug))

(package-initialize)

(add-to-list 'load-path "/home/paul/git/org-mode/lisp")
(add-to-list 'load-path "/home/paul/git/org-mode/contrib/lisp")
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(use-package org
    :ensure t)
;	     :ensure org-plus-contrib
;	     :load-path "~/git/org-mode"
;	     :demand t)

(defvar init-dir) ;; Initial directory for emacs configuration
(setq init-dir (file-name-directory (or load-file-name (buffer-file-name))))

;; Save emacs customisations in this file:
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

(org-babel-load-file
 (expand-file-name "emacs-init.org"
		   user-emacs-directory))

;;; init.el ends here


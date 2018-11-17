;;; init.el --- Startup file for emacs

;;; Commentary:
;; Initial configuration file loaded by Emacs.  Sets the custom
;; configuration file location, then loads the code in emacs-init.org
;; (c) 2017 - 2018 Paul Jewell
;; Licence: BSD

;;; Code:

(package-initialize)

(defvar init-dir) ;; Initial directory for emacs configuration
(setq init-dir (file-name-directory (or load-file-name (buffer-file-name))))

;; Save emacs customisations in this file:
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

(org-babel-load-file
  (expand-file-name "emacs-init.org"
                    user-emacs-directory))

;;; init.el ends here


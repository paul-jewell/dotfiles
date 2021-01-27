;;; package ---  early-init.el: Prevent package-initialize being called before loading the init file
;;; Commentary:

;;; Code:

(if (> emacs-major-version 27)
    (setq package-enable-at-startup nil))


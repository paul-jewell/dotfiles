;;; tangle-dotfiles --- extract dot files from org files.
;;; Commentary:
;;; System configuration files are stored in org files under
;;; ~/dotfiles and this code will extract them.

;;; Code:

(require 'org)
;; TODO: Re-evaluate how local settings are managed. Currently, they
;; are in the main emacs.org file, and move to the tangled output files
;;(load-file "~/.dotfiles/.emacs.d/lisp/pj-settings.el")

;; Don't ask when evaluating code blocks
(setq org-confirm-babel-evaluate nil)

(let* ((dotfiles-path (expand-file-name "~/.dotfiles"))
  (org-files (directory-files dotfiles-path nil "\\.org$")))

  (defun pj/tangle-org-file (org-file)
    (message "\n\033[1;32mUpdating %s\033[0m\n" org-file)
    (org-babel-tangle-file (expand-file-name org-file dotfiles-path)))

  (dolist (org-file org-files)
    (unless (member org-file '("README.org"))
      (pj/tangle-org-file org-file))))

(provide 'tangle-dotfiles)
;;; tangle-dotfiles.el ends here

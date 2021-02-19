;;; tangle-dotfiles.el --- Tangle config files
;;; Commentary:
;;; Tangle files to setup configuration.
;;; Code:

(require 'org)

;; Don't ask when evaluating code blocks
(setq org-confirm-babel-evaluate nil)

(let* ((dotfiles-path (expand-file-name "~/dotfiles"))
  (org-files (directory-files dotfiles-path nil "\\.org$")))

  (defun pj/tangle-org-file (org-file)
    (message "\n\033[1;32mUpdating %s\033[0m\n" org-file)
    (org-babel-tangle-file (expand-file-name org-file dotfiles-path)))

  ;; Tangle Systems.org first
  (pj/tangle-org-file "systems.org")

  (dolist (org-file org-files)
    (unless (member org-file '("README.org" "systems.org"))
      (pj/tangle-org-file org-file))))

;;; (provide 'tangle-dotfiles)
;;; tangle-dotfiles.el ends here

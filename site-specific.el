;;; Site-specific.el --- local variables for each system
;;; commentary:
;;; code:

;; Globals to configure which blocks are loaded

(defvar *pj/enable-mu4e-mode* nil   "Enable mu4e mode.")
(defvar *pj/load-site-gentoo* nil   "Load gentoo's config file.")
(defvar *pj/enable-auctex*    nil   "Enable auctex mode.")
(defvar *pj/font-size*        "10"  "Fontsize for this system.")

(defvar *pj/info-default-directory-list* "~/Nextcloud/git/org-mode/doc")

(defun pj/is-windows-p ()
  "True if run in windows environment."
  (string= "windows-nt" system-type))

(defun pj/is-linux-p ()
  "True if run in linux environment."
  (string= "gnu/linux" system-type))

;; Three possibilities for specifying values:

;; - Globally, for all systems
;; - By operating system
;; - By system name

(cond
 ((pj/is-linux-p)
  (if (string-prefix-p "DESKTOP" (system-name)) ;; Windows WSL2 on Tristan
      (progn
        (require 'gnutls)
        (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
	     (setq *pj/enable-mu4e-mode* t)
	     (setq *pj/load-site-gentoo* nil)
	     (setq *pj/enable-auctex* t)
	     
	     ;; define the location of the orgmode code
	     (add-to-list 'load-path "/mnt/c/Users/paul/Nextcloud/git/org-mode/lisp")
	     (add-to-list 'load-path "/mnt/c/Users/paul/Nextcloud/git/org-mode/contrib/lisp")
        (defvar *pj/org-agenda-files* '("/mnt/c/Users/paul/Nextcloud/org"))
        (defvar *pj/org-roam-directory*   "/mnt/c/Users/paul/Nextcloud/git/org/roam/")
        (defvar *pj/org-roam-db-location* "/mnt/c/Users/paul/Nextcloud/git/org/org-roam.db")
	     (setq *pj/font-size* "10"))
    (progn
      (setq *pj/enable-mu4e-mode* t)
      (setq *pj/load-site-gentoo* t)
      (setq *pj/enable-auctex* t)
      (defvar *pj/org-agenda-files* '("~/Nextcloud/org"))
      (defvar *pj/org-roam-directory*   "~/Nextcloud/git/org/roam/")
      (defvar *pj/org-roam-db-location* "~/Nextcloud/git/org/org-roam.db"))))
 ((pj/is-windows-p) ;; Not WSL2 installation - that is declared as linux
  (progn
    (setq *pj/enable-mu4e-mode* nil)
    (setq *pj/load-site-gentoo* nil)
    (setq *pj/enable-auctex* nil)
    
    ;; define the location of the orgmode code
    (add-to-list 'load-path  "c:/users/paul/Nextcloud/git/org-mode/lisp")
    (add-to-list 'load-path "c:/users/Paul/Nextcloud/git/org-mode/contrib/lisp")
    (defvar *pj/my-org-roam-directory* "c:/users/Paul/Nextcloud/git/org/roam/")
    (defvar *pj/org-agenda-files* '("~/Nextcloud/org"))
    (defvar *pj/org-roam-directory*   "~/Nextcloud/git/org/roam/")
    (defvar *pj/org-roam-db-location* "~/Nextcloud/git/org/org-roam.db")
    (setq *pj/font-size* "10")))
 (t
  (error "Undefined system-type %s" system-type)))

(provide 'site-specific)
;;; site-specific.el ends here

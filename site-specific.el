;;; Site-specific.el --- local variables for each system
;;; commentary:
;;; code:

;; Globals to configure which blocks are loaded

(defvar *pj/enable-mu4e-mode* t     "Enable mu4e mode.")
(defvar *pj/load-site-gentoo* t     "Load gentoo's config file.")
(defvar *pj/enable-auctex*    t     "Enable auctex mode.")
(defvar *pj/font-size*        "10"  "Fontsize for this system.")

(defvar *pj/info-default-directory-list* "~/Nextcloud/git/org-mode/doc")

(defun pj/is-windows-p ()
  (string= "windows-nt" system-type))

(defun pj/is-linux-p ()
  (string= "gnu/linux" system-type))

;; Three possibilities for specifying values:

;; - Globally, for all systems
;; - By operating system
;; - By system name

(cond
 ((is-linux-p)
  (cond ((string= "q"))) (progn
    (setq *pj/enable-mu4e-mode* t)
    (setq *pj/load-site-gentoo* t)
    (setq *pj/enable-auctex* t)
    
    ;; define the location of the orgmode code
    (add-to-list 'load-path  "/home/paul/git/org-mode/lisp")
    (add-to-list 'load-path "/home/paul/git/org-mode/contrib/lisp")
    (setq *pj/font-size* "10")))

 ((is-windows-p) ;; Not WSL2 installation - that is declared as linux
  (progn
    (setq *pj/enable-mu4e-mode* nil)
    (setq *pj/load-site-gentoo* nil)
    (setq *pj/enable-auctex* nil)
    
    ;; define the location of the orgmode code
    (add-to-list 'load-path  "/home/paul/git/org-mode/lisp")
    (add-to-list 'load-path "/home/paul/git/org-mode/contrib/lisp")
    (defvar *pj/my-org-roam-directory* "c:/users/Paul/Nextcloud/git/org/roam/")
    (setq *pj/font-size* "10")))
 (t
  (error "Undefined system-type %s" system-type)))

;; (cond
;;  ((string= "tristan" (system-name))
;;   (progn
;;     (setq *pj/enable-mu4e-mode* t)
;;     (setq *pj/load-site-gentoo* t)
;;     (setq *pj/enable-auctex* t)
    
;;     ;; define the location of the orgmode code
;;     (add-to-list 'load-path  "/home/paul/git/org-mode/lisp")
;;     (add-to-list 'load-path "/home/paul/git/org-mode/contrib/lisp")
;;     (setq *pj/font-size* "10"))
;;   (string= "shingo" (system-name))))


;; define local over rides for org-mode folders
;;(setq org-mobile-directory "~/Dropbox/MobileOrg")
(defvar *pj/org-agenda-files* '("~/Nextcloud/org"))
(defvar *pj/org-roam-directory*   "~/Nextcloud/git/org/roam/")
(defvar *pj/org-roam-db-location* "~/Nextcloud/git/org/org-roam.db")


(provide 'site-specific)
;;; site-specific.el ends here

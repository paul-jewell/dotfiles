;;; init.el --- Startup file for emacs

;;; Commentary:
;; Initial configuration file loaded by Emacs.
;;  - Sets the custom configuration file location
;; (c) 2017 - 2021 Paul Jewell
;; Licence: BSD

;;; Code:

;; Load machine local definitions
(load-file "~/.emacs.d/site-specific.el")

(require 'site-specific "~/.emacs.d/site-specific.el")

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

;;==============================================================================
;;.....General configuration
;;     ---------------------

(if *pj/load-site-gentoo*
    (require 'site-gentoo))

;; Set default modes
(setq major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-screen t)

;; dont use tabs for indenting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 3)
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)
(set-variable 'confirm-kill-emacs 'yes-or-no-p)

;; Eliminate C-z sleep
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; The following lines are always needed. Choose your own keys.
(global-font-lock-mode t)
(global-set-key "\C-x\C-l" 'goto-line)
(global-set-key "\C-x\C-y" 'copy-region-as-kill)

;; Remove the tool-bar from the top
(tool-bar-mode -1)
;; (menu-bar-mode -1)
(scroll-bar-mode -1)

;; Full path in title bar
(setq-default frame-title-format "%b (%f)")

(defalias 'list-buffers 'ibuffer)

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "Backups"))))

;;==============================================================================
;;.....Package management
;;     ------------------

;;(require 'cl)
(require 'gnutls)

(setq tls-checktrust t)

(defvar python)
(setq python (executable-find "python"))

(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string (concat python " -m certifi"))))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))

(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(defvar melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))
(defvar org-elpa '("org" . "https://orgmode.org/elpa/"))

;; Add marmalade to package repos
(setq package-archives nil)
(add-to-list 'package-archives melpa-stable t)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)
(add-to-list 'package-archives org-elpa t)

(initialise-packages)

(unless (and (file-exists-p (concat init-dir "elpa/archives/gnu"))
             (file-exists-p (concat init-dir "elpa/archives/melpa"))
             (file-exists-p (concat init-dir "elpa/archives/melpa-stable"))
             (file-exists-p (concat init-dir "elpa/archives/org")))
  (package-refresh-contents))

(defun packages-install (&rest packages)
  "Install PACKAGES."
  (message "running packages-install")
  (mapc (lambda (package)
          (let ((name (car package))
                (repo (cdr package)))
            (when (not (package-installed-p name))
              (let ((package-archives (list repo)))
                (initialise-packages)
                (package-install name)))))
        packages)
  (initialise-packages)
  (delete-other-windows))

;; Install extensions if they're missing
(defun init--install-packages ()
  "Install some packages."
  (message "Lets install some packages")
  (packages-install
   ;; Since use-package this is the only entry here
   ;; ALWAYS try to use use-package!
   (cons 'use-package melpa)
   ))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;;==============================================================================
;;.....Ivy
;;     ---

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)

  (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist)
  (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
  (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)

  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))

(use-package ivy-hydra
  :defer t
  :after hydra)

(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode 1)
  :config
  (setcdr  (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'ivy-switch-buffer
                   '(:columns
                     ((ivy-rich-candidate (:width 40))
                      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
                      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
                      (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
                      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
                     :predicate
                     (lambda (cand)
                       (if-let ((buffer (get-buffer cand)))
                           ;; Don't mess with EXWM buffers
                           (with-current-buffer buffer
                             (not (derived-mode-p 'exwm-mode)))))))))

;;==============================================================================
;;.....Swiper
;;     ------

;; Counsel - completion package working with ivy.
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

;; TODO: Configure counsel-bbdb to work eith email, or configure a different
;;       package to manage contacts (synced with cardDAV)
(use-package counsel-bbdb
  :ensure t)

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

;;==============================================================================
;;.....which-key
;;     ---------
;; Key completion - offers the keys which complete the sequence.

(use-package which-key
  :ensure t
  :config (which-key-mode))

;;==============================================================================
;;.....flyspell
;;     --------
;; Spell checker.

(setenv "LANG" "en_GB")
(setq ispell-program-name "hunspell")
(if (string= system-type "windows-nt")
    (setq ispell-hunspell-dict-paths-alist
          '(("en_GB" "c:/Hunspell/en_GB.aff"))))
(setq ispell-local-dictionary "en_GB")
(setq ispell-local-dictionary-alist
      '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8)))
;; (flyspell-mode 1)
(global-set-key (kbd "M-\\") 'ispell-word)

;;==============================================================================
;;.....ledger
;;     ------
;; Text based accounting program.

(use-package ledger-mode
  :ensure t
  :init
  (setq ledger-clear-whole-transactions 1)
  
  :config
  (add-to-list 'auto-mode-alist '("\\.dat$" . ledger-mode))
  (add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode)))

;;==============================================================================
;;.....go
;;     --
;; Package for go programming.

(use-package go-autocomplete
  :ensure t)
(use-package go-mode
   :ensure t
   :config
   (add-hook 'go-mode-hook (lambda () (setq auto-complete-mode 1)))
   (with-eval-after-load 'go-mode
     (require 'go-autocomplete)))

;;==============================================================================
;;.....Python
;;     ------


;;; Currently commented out - jedi mode should not be installed when using
;;; company mode. company-jedi should be used instead

;;(use-package jedi
;;  :ensure t
;;  :init
;;  (add-hook 'python-mode-hook 'jedi:setup)
;;  (add-hook 'python-mode-hook 'jedi:ac-setup))
;;; Alternative - use elpy - not yet fully configured
;;(use-package elpy
;;  :ensure t
;;  :init
;;  (advice-add 'python-mode :before 'elpy-enable))

;;==============================================================================
;;.....SQL
;;     ---


(require 'sql)

(eval-after-load "sql"
  '(progn (sql-set-product 'mysql)))


;;==============================================================================
;;.....c++
;;     ---

(defun my-c++-mode-hook()
  "Customise the default c++ settings."
  (c-set-style "stroustrup"))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;;==============================================================================
;;.....smex
;;     ----
;; M-x enhancement - show most recently used commands which match as typing.

(use-package smex
:ensure t
:bind (("M-x" . smex)
       ("M-X" . smex-major-mode-commands)
       ("C-c C-c M-x" . 'execute-extended-command)) ;; Original M-x command
:config (smex-initialize))

(defadvice ido-set-matches-1 (around ido-smex-acronym-matches activate)
  "Filters ITEMS by setting acronynms first."
  (if (and (fboundp 'smex-already-running) (smex-already-running) (> (length ido-text) 1))

      ;; We use a hash table for the matches, <type> => <list of items>, where
      ;; <type> can be one of (e.g. `ido-text' is "ff"):
      ;; - strict: strict acronym match (i.e. "^f[^-]*-f[^-]*$");
      ;; - relaxed: for relaxed match (i.e. "^f[^-]*-f[^-]*");
      ;; - start: the text start with (i.e. "^ff.*");
      ;; - contains: the text contains (i.e. ".*ff.*");
      (let ((regex (concat "^" (mapconcat 'char-to-string ido-text "[^-]*-")))
            (matches (make-hash-table :test 'eq)))

        ;; Filtering
        (dolist (item items)
          (let ((key))
            (cond
             ;; strict match
             ((string-match (concat regex "[^-]*$") item)
              (setq key 'strict))

             ;; relaxed match
             ((string-match regex item)
              (setq key 'relaxed))

             ;; text that start with ido-text
             ((string-match (concat "^" ido-text) item)
              (setq key 'start))

             ;; text that contains ido-text
             ((string-match ido-text item)
              (setq key 'contains)))

            (when key
              ;; We have a winner! Update its list.
              (let ((list (gethash key matches ())))
                (puthash key (push item list) matches)))))

        ;; Finally, we can order and return the results
        (setq ad-return-value (append (gethash 'strict matches)
                                      (gethash 'relaxed matches)
                                      (gethash 'start matches)
                                      (gethash 'contains matches))))

    ;; ...else, run the original ido-set-matches-1
    ad-do-it))

;; Delayed loading - initialisation when used for the first time
;; (global-set-key [(meta x)]
;;   (lambda ()
;;     (interactive)
;;     (or (boundp 'smex-cache)
;;         (smex-initialize))
;;     (global-set-key [(meta x)] 'smex) (smex)))

;; (global-set-key [(shift meta x)]
;;   (lambda () (interactive)
;;   (or (boundp 'smex-cache) (smex-initialize))
;;   (global-set-key [(shift meta x)] 'smex-major-mode-commands)
;;   (smex-major-mode-commands)))

;;==============================================================================
;;.....multiple cursors
;;     ----------------

(use-package multiple-cursors
  :ensure t
  :config (global-set-key (kbd "C-c m c") 'mc/edit-lines))

;;==============================================================================
;;.....org mode
;;     --------

(setq org-agenda-files *pj/org-agenda-files*)
(load "~/.emacs.d/lisp/my-org-mode.el")
(require 'org-habit)
(global-set-key (kbd "C-c w") 'org-refile)

;;==============================================================================
;;.....org bullet mode
;;     ---------------

(use-package org-bullets
  :ensure t
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;==============================================================================
;;.....org roam mode
;;     -------------

;; Installation advice from the org-roam documentation website:
;; https://org-roam.readthedocs.io/en/master/installation/

(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-db-location *pj/org-roam-db-location*)
  (org-roam-directory *pj/org-roam-directory*)
  (org-roam-index-file "index.org")
  ;; (setq org-roam-link-title-format "R:%s")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n j" . org-roam-jump-to-index)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))


;;==============================================================================
;;.....GPL3 File header boilerplate
;;     ----------------------------

(defun boilerplate-gpl3 ()
        (interactive)
        "Insert GPLv3 boilerplate"
        (insert "
/********************************************************************************
 * Copyright (C) " (format-time-string "%Y") " Paul Jewell (paul@teulu.org)                              *
 *                                                                              *
 * This program is free software: you can redistribute it and/or modify         *
 * it under the terms of the GNU General Public License as published by         *
 * the Free Software Foundation, either version 3 of the License, or            *
 * (at your option) any later version.                                          *
 *                                                                              *
 * This program is distributed in the hope that it will be useful,              *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of               *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                *
 * GNU General Public License for more details.                                 *
 *                                                                              *
 * You should have received a copy of the GNU General Public License            *
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.        *
 ********************************************************************************/
"))

(defun boilerplate-lgpl3 ()
        (interactive)
        "Insert LGPLv3 boilerplate"
        (insert "
/********************************************************************************
 * Copyright (C) " (format-time-string "%Y") " Paul Jewell (paul@teulu.org)                              *
 *                                                                              *
 * This program is free software: you can redistribute it and/or modify         *
 * it under the terms of the GNU Lesser General Public License as published by  *
 * the Free Software Foundation, either version 3 of the License, or            *
 * (at your option) any later version.                                          *
 *                                                                              *
 * This program is distributed in the hope that it will be useful,              *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of               *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                *
 * GNU Lesser General Public License for more details.                          *
 *                                                                              *
 * You should have received a copy of the GNU Lesser General Public License     *
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.        *
 ********************************************************************************/
"))
(defun boilerplate-agpl3 ()
        (interactive)
        "Insert AGPLv3 boilerplate"
        (insert "
/********************************************************************************
 * Copyright (C) " (format-time-string "%Y") " Paul Jewell (paul@teulu.org)                              *
 *                                                                              *
 * This program is free software: you can redistribute it and/or modify         *
 * it under the terms of the GNU Affero General Public License as published by  *
 * the Free Software Foundation, either version 3 of the License, or            *
 * (at your option) any later version.                                          *
 *                                                                              *
 * This program is distributed in the hope that it will be useful,              *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of               *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                *
 * GNU Affero General Public License for more details.                          *
 *                                                                              *
 * You should have received a copy of the GNU Affero General Public License     *
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.        *
 ********************************************************************************/
"))

(defun insert-timestamp ()
  (interactive)
  "Inserts a timestamp"
  (insert (format-time-string "%Y%m%d.%H%M%S%z/%s")))

;;==============================================================================
;;.....auctex
;;     ------

(when *pj/enable-auctex*
  (use-package auctex
    :ensure t
    :mode ("\\.tex\\'" . latex-mode)
    :config
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq-default TeX-master nil)
    
    (add-hook 'LaTeX-mode-hook 
              (lambda ()
                (company-mode)
                (visual-line-mode) ; May prefer auto-fill-mode
                (flyspell-mode)
                (turn-on-reftex)
                (setq TeX-PDF-mode t)
                (setq reftex-plug-into-AUCtex t)
                (LaTeX-math-mode)))
    
    ;; Update PDF buffers after successful LaTaX runs
    (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
              #'TeX-revert-document-buffer)
    
    ;; to use pdfview with auctex
    (add-hook 'Latex-mode-hook 'pdf-tools-install)))


;;==============================================================================
;;.....reftex
;;     ------

(use-package reftex
  :ensure t
  :defer t
  :config
  (setq reftex-cite-prompt-optional-args t)) ; prompt for empty optional args in cite


;;==============================================================================
;;.....ivy-bibtex
;;     ----------

;; TODO: Modify the paths etc in this section:

;;(use-package ivy-bibtex
;;  :ensure t
;;  :bind ("C-c b b" . ivy-bibtex)
;;  :config
;;  (setq bibtex-completion-bibliography 
;;        '("C:/Users/Nasser/OneDrive/Bibliography/references-zot.bib"))
;;  (setq bibtex-completion-library-path 
;;        '("C:/Users/Nasser/OneDrive/Bibliography/references-pdf"
;;          "C:/Users/Nasser/OneDrive/Bibliography/references-etc"))
;;
;;  ;; using bibtex path reference to pdf file
;;  (setq bibtex-completion-pdf-field "File")
;;
;;  ;;open pdf with external viwer foxit
;;  (setq bibtex-completion-pdf-open-function
;;        (lambda (fpath)
;;          (call-process "C:\\Program Files (x86)\\Foxit Software\\Foxit Reader\\FoxitReader.exe" nil 0 nil fpath)))
;;
;;  (setq ivy-bibtex-default-action 'bibtex-completion-insert-citation))


;;==============================================================================
;;.....hydra
;;     -----

(use-package hydra 
  :ensure hydra
  :init 
  (global-set-key
   (kbd "C-x t")
	(defhydra toggle (:color blue)
	  "toggle"
	  ("a" abbrev-mode "abbrev")
	  ("s" flyspell-mode "flyspell")
	  ("d" toggle-debug-on-error "debug")
     ;;	      ("c" fci-mode "fCi")
	  ("f" auto-fill-mode "fill")
	  ("t" toggle-truncate-lines "truncate")
	  ("w" whitespace-mode "whitespace")
	  ("q" nil "cancel"))))
(global-set-key
 (kbd "C-x j")
 (defhydra gotoline 
   ( :pre (linum-mode 1)
	       :post (linum-mode -1))
   "goto"
   ("t" (lambda () (interactive)(move-to-window-line-top-bottom 0)) "top")
   ("b" (lambda () (interactive)(move-to-window-line-top-bottom -1)) "bottom")
   ("m" (lambda () (interactive)(move-to-window-line-top-bottom)) "middle")
   ("e" (lambda () (interactive)(end-of-buffer)) "end")
   ("c" recenter-top-bottom "recenter")
   ("n" next-line "down")
   ("p" (lambda () (interactive) (forward-line -1))  "up")
   ("g" goto-line "goto-line")
   ))

    ;;    (global-set-key
;;     (kbd "C-c t")
;;     (defhydra hydra-global-org (:color blue)
;;       "Org"
;;       ("t" org-timer-start "Start Timer")
;;       ("s" org-timer-stop "Stop Timer")
;;       ("r" org-timer-set-timer "Set Timer") ; This one requires you be in an orgmode doc, as it sets the timer for the header
;;       ("p" org-timer "Print Timer") ; output timer value to buffer
;;       ("w" (org-clock-in '(4)) "Clock-In") ; used with (org-clock-persistence-insinuate) (setq org-clock-persist t)
;;       ("o" org-clock-out "Clock-Out") ; you might also want (setq org-log-note-clock-out t)
;;       ("j" org-clock-goto "Clock Goto") ; global visit the clocked task
;;       ("c" org-capture "Capture") ; Don't forget to define the captures you want http://orgmode.org/manual/Capture.html
;;     ("l" (or )rg-capture-goto-last-stored "Last Capture"))
    
    

;; (defhydra multiple-cursors-hydra (:hint nil)
;;   "
;;      ^Up^            ^Down^        ^Other^
;; ----------------------------------------------
;; [_p_]   Next    [_n_]   Next    [_l_] Edit lines
;; [_P_]   Skip    [_N_]   Skip    [_a_] Mark all
;; [_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
;; ^ ^             ^ ^             [_q_] Quit
;; "
;;   ("l" mc/edit-lines :exit t)
;;   ("a" mc/mark-all-like-this :exit t)
;;   ("n" mc/mark-next-like-this)
;;   ("N" mc/skip-to-next-like-this)
;;   ("M-n" mc/unmark-next-like-this)
;;   ("p" mc/mark-previous-like-this)
;;   ("P" mc/skip-to-previous-like-this)
;;   ("M-p" mc/unmark-previous-like-this)
;;   ("r" mc/mark-all-in-region-regexp :exit t)
;;   ("q" nil)

;;   ("<mouse-1>" mc/add-cursor-on-click)
;;   ("<down-mouse-1>" ignore)
;;   ("<drag-mouse-1>" ignore))


;; font zoom mode example taken from hydra wiki
(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("+" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("0" (text-scale-adjust 0) "reset")
  ("q" nil "quit" :color blue))

;;==============================================================================
;;.....javascript / HTML
;;     -----------------

(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-hook 'js-mode-hook 'subword-mode)
(add-hook 'html-mode-hook 'subword-mode)
(setq js-indent-level 2)
(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))

;; Coffeescript
(add-to-list 'auto-mode-alist '("\\.coffee.erb$" . coffee-mode))
(add-hook 'coffee-mode-hook 'subword-mode)
(add-hook 'coffee-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'coffee-mode-hook
          (defun coffee-mode-newline-and-indent ()
            (define-key coffee-mode-map "\C-j" 'coffee-newline-and-indent)
            (setq coffee-cleanup-whitespace nil)))
(custom-set-variables
 '(coffee-tab-width 2))

;;==============================================================================
;;.....company mode
;;     ------------

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (global-company-mode 1))

(use-package company-irony
  :ensure t
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package irony-eldoc
  :ensure t
  :config
  (add-hook 'irony-mode-hook #'irony-eldoc))

(use-package company-jedi
  :ensure t
  :config
  (add-hook 'python-mode-hook 'jedi:setup))

(defun my/python-mode-hook ()
  "Python mode hook."
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

;;==============================================================================
;;.....magit
;;     -----

(use-package magit
  :ensure t
  :init
  (progn
    (bind-key "C-x g" 'magit-status)
    ))

(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode +1))

(global-set-key (kbd "M-g M-g") 'hydra-git-gutter/body)


(use-package git-timemachine
  :ensure t
  )
(defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                                      :hint nil)
  "
Git gutter:
  _j_: next hunk        _s_tage hunk     _q_uit
  _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  ^ ^                   _p_opup hunk
  _h_: first hunk
  _l_: last hunk        set start _R_evision
"
  ("j" git-gutter:next-hunk)
  ("k" git-gutter:previous-hunk)
  ("h" (progn (goto-char (point-min))
              (git-gutter:next-hunk 1)))
  ("l" (progn (goto-char (point-min))
              (git-gutter:previous-hunk 1)))
  ("s" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  ("p" git-gutter:popup-hunk)
  ("R" git-gutter:set-start-revision)
  ("q" nil :color blue)
  ("Q" (progn (git-gutter-mode -1)
              ;; git-gutter-fringe doesn't seem to
              ;; clear the markup right away
              (sit-for 0.1)
              (git-gutter:clear))
   :color blue))

;;==============================================================================
;;.....flycheck
;;     --------

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode 1))

;;==============================================================================
;;.....all the icons
;;     -------------


;; If this configuration is being used on a new installation,
;; remember to run M-x all-the-icons-install-fonts
;; otherwise nothing will work
(use-package all-the-icons
:ensure t
:config
(use-package all-the-icons-dired
    :ensure t
    :config
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))


;;==============================================================================
;;.....themes
;;     ------

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox t))
;; Font size is localised in site-local.el
(defvar my:font (concat "Iosevka-" *pj/font-size* ":spacing=110"))
;; Font size setting for Emacs 27:
(set-face-attribute 'default nil :font my:font )
(set-frame-font my:font nil t)
;; Old font size setting:
;;(set-default-font my:font)
;;(set-frame-font my:font t)

;;==============================================================================
;;.....eyebrowse
;;     ---------

;; TODO: currently disabled - clash with org-refile needs to be resolved.
;;(use-package eyebrowse
;;  :ensure r
;;  :config
;;;;  (eyebrowse-setup-opinionated-keys) ;set evil keybindings (gt gT)
;;  (eyebrowse-mode t))

;;==============================================================================
;;.....Projectile
;;     ----------

(use-package projectile
  :ensure t
  :config
  ;; test fn in hashtabe has to be equal because we will use strings as keys
  (setq my-projects-loaded (make-hash-table :test 'equal))
  (setq projectile-completion-system 'ivy)
  (projectile-global-mode))


;;==============================================================================
;;.....powerline
;;     ---------

(use-package powerline
  :ensure t
  :config
  (add-hook 'desktop-after-read-hook 'powerline-reset)
  (defun make-rect (color height width)
    "Create an XPM bitmap."
    (when window-system
      (propertize
       " " 'display
       (let ((data nil)
             (i 0))
         (setq data (make-list height (make-list width 1)))
         (pl/make-xpm "percent" color color (reverse data))))))
  (defun powerline-mode-icon ()
    (let ((icon (all-the-icons-icon-for-buffer)))
      (unless (symbolp icon) ;; This implies it's the major mode
        (format " %s"
                (propertize icon
                            'help-echo (format "Major-mode: `%s`" major-mode)
                            'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))))
  (defun powerline-modeline-vc ()
    (when vc-mode
      (let* ((text-props (text-properties-at 1 vc-mode))
             (vc-without-props (substring-no-properties vc-mode))
             (new-text (concat
                        " "
                        (all-the-icons-faicon "code-fork"
                                              :v-adjust -0.1)
                        vc-without-props
                        " "))
             )
        (apply 'propertize
               new-text
               'face (when (powerline-selected-window-active) 'success)
               text-props
               ))))
  (defun powerline-buffer-info ()
    (let ((proj (projectile-project-name)))
      (if (string= proj "-")
          (buffer-name)
        (concat
         (propertize (concat
                      proj)
                     'face 'warning)
         " "
         (buffer-name)))))
  (defun powerline-ace-window () (propertize (or (window-parameter (selected-window) 'my-ace-window-path) "") 'face 'error))
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (modified (buffer-modified-p))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (bar-color (cond ((and active modified) (face-foreground 'error))
                                           (active (face-background 'cursor))
                                           (t (face-background 'tooltip))))
                          (lhs (list
                                (make-rect bar-color 30 3)
                                (when modified
                                  (concat
                                   " "
                                   (all-the-icons-faicon "floppy-o"
                                                         :face (when active 'error)
                                                         :v-adjust -0.01)))
                                " "
                                (powerline-buffer-info)
                                " "
                                (powerline-modeline-vc)
                                ))
                          (center (list
                                   " "
                                   (powerline-mode-icon)
                                   " "
                                   ;;major-mode
                                   (powerline-major-mode)
                                   " "))
                          (rhs (list
                                (powerline-ace-window)
                                " | "
                                ;;   (format "%s" (eyebrowse--get 'current-slot))
                                ;;   " | "
                                (powerline-raw "%l:%c" face1 'r)
                                " | "
                                (powerline-raw "%6p" face1 'r)
                                (powerline-hud 'highlight 'region 1)
                                " "
                                ))
                          )
                     (concat
                      (powerline-render lhs)
                      (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                      (powerline-render center)
                      (powerline-fill face2 (powerline-width rhs))
                      (powerline-render rhs)))))))

(require 'diminish)


;;==============================================================================
;;.....Paredit
;;     -------

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
  :bind (("C-c d" . paredit-forward-down))
  )

;; Ensure paredit is used EVERYWHERE!
(use-package paredit-everywhere
  :ensure t
  :diminish paredit-everywhere-mode
  :config
  (add-hook 'lisp-mode-hook #'paredit-everywhere-mode))

(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda()
              (highlight-parentheses-mode))))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'lisp-mode-hook
            (lambda()
              (rainbow-delimiters-mode))))

(global-highlight-parentheses-mode)

;;==============================================================================
;;.....Clojure
;;     -------

(add-hook 'clojure-mode-hook 'enable-paredit-mode)

(use-package cider
  :ensure t
  ;;:pin melpa-stable

  :config
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-mode-hook #'cider-hydra-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-repl-display-help-banner nil)
  (setq cider-default-cljs-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

  :bind (("M-r" . cider-namespace-refresh)
         ("C-c r" . cider-repl-reset)
         ("C-c ." . cider-reset-test-run-tests)))


;; (use-package clj-refactor
;;   :ensure t
;;   :config
;;   (add-hook 'clojure-mode-hook (lambda ()
;;                                  (clj-refactor-mode 1)
;;                                  ;; insert keybinding setup here
;;                                  ))
;;   (cljr-add-keybindings-with-prefix "C-c C-m")
;;   (setq cljr-warn-on-eval nil)
;;   :bind ("C-c '" . hydra-cljr-help-menu/body)
;;   )

(use-package cider-hydra
  :ensure t)


;;==============================================================================
;;.....lisp - slime
;;     ------------

;; shamelessly copied from 
;; https://github.com/ajukraine/ajukraine-dotemacs/blob/master/aj/rc-modes/init.el
;; 17/11/2018

(use-package slime
;;  :load-path (expand-site-lisp "slime")
  :ensure t
  :commands slime
  :config

  (progn
    (add-hook
     'slime-load-hook
     #'(lambda ()
         (slime-setup
          '(slime-fancy
            slime-repl
            slime-fuzzy))))
    (setq slime-net-coding-system 'utf-8-unix)
    (setq inferior-lisp-program "/usr/bin/sbcl")
    (load (expand-file-name "~/quicklisp/slime-helper.el"))
    (setq slime-lisp-implementations '((sbcl ("/usr/bin/sbcl"))))
    
    (use-package ac-slime
      :ensure t
      :init
      (progn
        (add-hook 'slime-mode-hook 'set-up-slime-ac)
        (add-hook 'slime-repl-mode-hook 'set-up-slime-ac))
      :config
      (progn
        (eval-after-load "auto-complete"
          '(add-to-list 'ac-modes 'slime-repl-mode))))))

;;==============================================================================
;;.....elisp - slime
;;     -------------

(use-package elisp-slime-nav
  :ensure t
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-slime-nav-mode)))

;;==============================================================================
;;.....mu4e
;;     ----

(when *pj/enable-mu4e-mode*
  (require 'mu4e)
  (setq mail-user-agent 'mu4e-user-agent)
  (require 'mu4e-contrib)
  (setq mu4e-html2text-command 'mu4e-shr2text)
  
  (require 'smtpmail)
  
  (setq mu4e-maildir "/home/paul/mail/home")
                                        ;(setq
                                        ;   message-send-mail-function   'smtpmail-send-it
                                        ;   smtpmail-default-smtp-server "smtp.123-reg.co.uk"
                                        ;   smtpmail-smtp-server         "smtp.123-reg.co.uk"
                                        ;   smtpmail-local-domain        "teulu.org")
  
  (setq send-mail-function 'sendmail-send-it
        sendmail-program "/usr/local/bin/msmtp-enqueue.sh"
        mail-specify-envelope-from t
        message-sendmail-envelope-from 'header
        mail-envelope-from 'header)
  
  (setq mu4e-sent-folder   "/Sent")
  (setq mu4e-drafts-folder "/Drafts")
  (setq mu4e-trash-folder  "/Trash")
  
  (setq mu4e-get-mail-command "offlineimap"
        mu4e-html2text-command "w3m -T text/html"
        mu4e-update-interval 120
        mu4e-headers-auto-update t
        mu4e-compose-signature-auto-include nil)
  
                                        ; TODO:: Need to check folder names
  (setq mu4e-maildir-shortcuts
        '( ("/INBOX"      . ?i)
           ("/sent Items" . ?s)))
  
  (setq mu4e-show-images t)
  
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))
  
  (setq mu4e-sent-messages-behaviour 'delete)
  
  (add-hook 'mu4e-compose-mode-hook
            (defun my-do-compose-stuff ()
              "My settings for message composition"
              (auto-fill-mode -1)
              (flyspell-mode)))
  
  (setq
   user-mail-address "paul@teulu.org"
   user-full-name  "Paul Jewell"
   message-signature "Paul Jewell\n"))

;;==============================================================================
;;.....Ox-Hugo
;;     -----

(use-package ox-hugo
  :ensure t
  :after ox)

(provide 'init)
;;; init.el ends here

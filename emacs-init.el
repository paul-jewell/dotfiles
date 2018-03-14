(require 'site-gentoo)
;; Set default modes
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq inhibit-startup-screen t)

;; dont use tabs for indenting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 3)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)
(set-variable 'confirm-kill-emacs 'yes-or-no-p)

;; The following lines are always needed. Choose your own keys.
(global-font-lock-mode t)
(global-set-key "\C-x\C-r" 'prefix-region)
(global-set-key "\C-x\C-l" 'goto-line)
(global-set-key "\C-x\C-y" 'copy-region-as-kill)

;; Remove the tool-bar from the top
(tool-bar-mode -1)

(global-set-key (kbd "<f4>") 'revert-buffer)

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package))

(use-package which-key
:ensure t
:config (which-key-mode))

(use-package ledger-mode
:ensure t
:init
(setq ledger-clear-whole-transactions 1)

:config
(add-to-list 'auto-mode-alist '("\\.dat$" . ledger-mode))
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode)))

;; Set up go-mode
(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)

(with-eval-after-load 'go-mode
  (require 'go-auto complete))

;; scss mode
(setq scss-compile-at-save nil)

;; Default to mysql syntax for sql code
(eval-after-load "sql"
  '(progn (sql-set-product 'mysql)))

;; set c++-mode style default
(defun my-c++-mode-hook()
  ;;(add-to-list 'c-default-style '(c++/l . "stroustrup"))
  (c-set-style "stroustrup"))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; scss mode
(setq scss-compile-at-save nil)

;; private diary

;Stolen from the emacs wiki. :)
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (when (buffer-file-name buffer)
        (progn
          (set-buffer buffer)
          (revert-buffer t t t)))
      (setq list (cdr list))
      (setq buffer (car list))))
 (message "Refreshing open files"))

;; define local over rides for org-mode folders
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-agenda-files (quote ("~/git/org"
                               "~/git/org/toyota")))

;; org-mode configuration from Bernt Hansen - bernt@norang.ca
(load "~/.emacs.d/lisp/org-mode.el")
(require 'org-habit)
(semantic-mode 1)

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
(add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

;; This code is deprecated - since the move to melpa managed themes
;;(if (display-graphic-p)
;;    (progn
;;      (require 'color-theme)
;;      (color-theme-initialize)
;;      (color-theme-gruvbox)
;;      (color-theme-calm-forest)
;;      (color-theme-solarized-dark)
;;      (set-face-attribute 'default nil :font "gohufont-14")
;;      (set-frame-size (selected-frame) 100 80)
;;	))

(load-theme 'darktooth 1)

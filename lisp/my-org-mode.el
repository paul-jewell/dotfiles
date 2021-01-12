;;; package --- Summary

;; Org-mode personal configuration

;;; Commentary:
;; Developed/inspired from Bernt Hansen's configuration
;; bernt@norang.ca
;; Errors are mine! - paulj

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'org-clock)
(require 'org-archive)
(require 'org-checklist)
(require 'org-crypt)
(require 'org-protocol)
(require 'ido)

;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


;; Custom Key Bindings
(global-set-key (kbd "<f12>") 'org-agenda)


(global-set-key (kbd "<F7>") 'pj/set-truncate-lines)

;; Cycles through the different org-mode agenda files (buffers)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)

;; TODO: Resolve bbdb issues
;; (global-set-key (kbd "<f9> b") 'bbdb)
(global-set-key (kbd "<f9> c") 'calendar)

;; Not using gnus
;; (global-set-key (kbd "<f9> g") 'gnus)

(global-set-key (kbd "<f9> n")   'pj/toggle-next-task-display)
(global-set-key (kbd "<f9> I")   'pj/punch-in)
(global-set-key (kbd "<f9> O")   'pj/punch-out)

(global-set-key (kbd "<f9> s")   'bh/switch-to-scratch)

(global-set-key (kbd "<f9> t")   'bh/insert-inactive-timestamp)
(global-set-key (kbd "<f9> T")   'bh/toggle-insert-inactive-timestamp)

(global-set-key (kbd "<f9> l")   'org-toggle-link-display)
(global-set-key (kbd "<f9> SPC") 'pj/clock-in-last-task)
(global-set-key (kbd "C-<f9>")   'previous-buffer)
(global-set-key (kbd "M-<f9>")   'org-toggle-inline-images)
(global-set-key (kbd "C-x n r")  'narrow-to-region)
(global-set-key (kbd "C-<f10>")  'next-buffer)
(global-set-key (kbd "<f11>")    'org-clock-goto)
(global-set-key (kbd "C-<f11>")  'org-clock-in)

(global-set-key (kbd "C-c c")    'org-capture)

(defun pj/set-truncate-lines ()
  "Toggle value of 'truncate-lines' and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(defun pj/switch-to-scratch ()
  "Switch to scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(setq org-use-fast-todo-selection 'auto)

(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(defvar *pj/org-directory* (car org-agenda-files))
(defvar *pj/org-default-notes-file* (concat (file-name-as-directory *pj/org-directory*) "refile.org"))
(defvar *pj/org-default-refile-file* (concat (file-name-as-directory *pj/org-directory*) "refile.org"))

;; I use C-c c to start capture mode

(global-set-key (kbd "C-c c") 'org-capture)

(defun pj/uuidgen ()
  "Generate a UUID for the ID Property of each task."
  (shell-command-to-string "uuidgen"))

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol

(defvar org-capture-templates
  (quote (("t" "todo" entry (file *pj/org-default-refile-file*)
           "* TODO %?
:PROPERTIES:
:ID:    %(pj/uuidgen):CREATED: %U\n%a
:END:" :prepend t :clock-in t :clock-resume t)
          
          ("n" "note" entry (file *pj/org-default-refile-file*)
           "* %? :NOTE:\n
:PROPERTIES:
:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %U\n%
:END:" :clock-in t :clock-resume t)

          ("j" "Journal" entry (file+datetree "~Nextcloud/git/org/diary.org.gpg")
           "* %?" :clock-in t :clock-resume t)
          
          ("w" "org-protocol" entry (file *pj/org-default-refile-file*)
           "* TODO Review %c\n
:PROPERTIES:
:ID: %(pj/uuidgen):CREATED: %U
:END:" :immediate-finish t)
          ("m" "Meeting" entry (file *pj/org-default-refile-file*)
           "* MEETING with %? :MEETING:
:PROPERTIES:
:ID: %(pj/uuidgen):CREATED: %U
:END:" :clock-in t :clock-resume t)

          ("p" "Phone call" entry (file *pj/org-default-refile-file*)
           "* PHONE %? :PHONE:
:PROPERTIES:
:ID: %(pj/uuidgen):CREATED: %U
:END:" :clock-in t :clock-resume t)

          ("h" "Habit" entry (file *pj/org-default-refile-file*)
           "* NEXT %?\n\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")
:PROPERTIES:
:ID: %(pj/uuidgen):CREATED: %U
:STYLE: habit
:REPEAT_TO_STATE: NEXT
:END:\n"))))

;; Remove empty LOGBOOK drawers on clock out

(defun pj/remove-empty-drawer-on-clock-out ()
  "Remove empty LOGBOOK drawers on clock out."
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))

(add-hook 'org-clock-out-hook 'pj/remove-empty-drawer-on-clock-out 'append)


; Targets include this file and any file contributing to the agenda - up to 9 levels deep

(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

;; Use full outline paths for refile targets - we file directly with IDO

(setq org-refile-use-outline-path t)

;; Targets complete directly with IDO

(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation

(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t

(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
; Use the current window when visiting files and buffers with ido

(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
; Use the current window for indirect buffer display

(setq org-indirect-buffer-display 'current-window)

;;;; Refile settings
; Exclude DONE state tasks from refile targets

(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; Do not dim blocked tasks

(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view

(setq org-agenda-compact-blocks t)

;; Custom agenda command definitions

(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              (" " "Agenda"
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-agenda-skip-function 'pj/skip-non-stuck-projects)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-HOLD-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'pj/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                  (if pj/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'pj/skip-projects-and-habits-and-single-tasks)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-scheduled pj/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines pj/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date pj/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                  (if pj/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'pj/skip-non-project-tasks)
                            (org-agenda-todo-ignore-scheduled pj/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines pj/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date pj/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                  (if pj/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'pj/skip-project-tasks)
                            (org-agenda-todo-ignore-scheduled pj/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines pj/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date pj/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED+WAITING|HOLD/!"
                           ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                  (if pj/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'pj/skip-non-tasks)
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-scheduled pj/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines pj/hide-scheduled-and-waiting-next-tasks)))
                (tags "-REFILE/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'pj/skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil))))
               nil))))

(defun pj/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda if TAG is \"hold\"  with / RET."
  (cond
   ((string= tag "hold")
    t))
  (concat "-" tag))

(setq org-agenda-auto-exclude-function 'pj/org-auto-exclude-function)

;;
;; Resume clocking task when emacs is restarted

(org-clock-persistence-insinuate)
;;
;; Show lot of clocking history so it's easy to pick items off the C-F11 list

(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open

(setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in

(setq org-clock-in-switch-to-state 'pj/clock-in-to-next)
;; Separate drawers for clocking and logs

(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer

(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration

(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state

(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup

(setq org-clock-persist t)
;; Do not prompt to resume an active clock

(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks

(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports

(setq org-clock-report-include-clocking-task t)

(defvar *pj/keep-clock-running* nil)

;; TODO: according to the documentation, the parameter should be the
;; state of the task - not sure why this code is fetching the state
;; using org-get-todo-state. Maybe no need...


(defun pj/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in (KW not used).
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (pj/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (pj/is-project-p))
      "TODO"))))

(defun pj/find-project-task ()
  "Move point to the parent (project) task if any."
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun pj/punch-in (arg)
  "Start continuous clocking; default task set to the selected task (ARG).
If no task is selected set the Organization task as the default task."
  (interactive "p")
  (setq *pj/keep-clock-running* t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (pj/clock-in-organisation-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
                                        ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p))
               (eq arg 4))
          (org-clock-in '(16))
        (pj/clock-in-organisation-task-as-default)))))

(defun pj/punch-out ()
  "Clock out current task."
  (interactive)
  (setq *pj/keep-clock-running* nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun pj/clock-in-default-task ()
  "Clock the default task in."
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun pj/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in."
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when *pj/keep-clock-running*
            (pj/clock-in-default-task)))))))

(defvar pj/organisation-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

(defun pj/clock-in-organisation-task-as-default ()
  "Clock organisation task in as default task."
  (interactive)
  (org-with-point-at (org-id-find pj/organisation-task-id 'marker)
    (org-clock-in '(16))))

(defun pj/clock-out-maybe ()
  "Clock in parent task under the appropriate circumstances."
  (when (and *pj/keep-clock-running*
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (pj/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'pj/clock-out-maybe 'append)

(require 'org-id)
(defun pj/clock-in-task-by-id (id)
  "Clock in a task by ID."
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))

(defun pj/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one (ARG).
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

(setq org-time-stamp-rounding-minutes (quote (1 1)))

(setq org-agenda-clock-consistency-checks
      (quote (:max-duration "4:00"
                            :min-duration 0
                            :max-gap 0
                            :gap-ok-around ("4:00"))))

;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration

(setq org-clock-out-remove-zero-time-clocks t)

;; Agenda clock report parameters

(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

; Set default column view headings: Task Effort Clock_Summary

(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

; global Effort estimate values
; global STYLE property values for completion

(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit"))))

;; Agenda log mode items to display (closed and state changes by default)

(setq org-agenda-log-mode-items (quote (closed state)))

; Tags with fast selection keys

(setq org-tag-alist (quote ((:startgroup)
                            ("@errand" . ?e)
                            ("@office" . ?o)
                            ("@home" . ?H)
                            (:endgroup)
                            ("WAITING" . ?w)
                            ("HOLD" . ?h)
                            ("PERSONAL" . ?P)
                            ("WORK" . ?W)
                            ("ORG" . ?O)
                            ("crypt" . ?E)
                            ("NOTE" . ?n)
                            ("CANCELLED" . ?c)
                            ("FLAGGED" . ??))))

; Allow setting single tags without the menu

(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates

(setq org-agenda-tags-todo-honor-ignore-options t)

(use-package bbdb
  :ensure t
  :config
  (bbdb-initialize))

(require 'bbdb-com)

(global-set-key (kbd "<f9> p") 'pj/phone-call)

;; Phone capture template handling with BBDB lookup
;; Adapted from code by Gregory J. Grubbs
;;
;; TODO: Work out how to populate bbdb database with names and details
;;       of contacts.

(defun pj/phone-call ()
  "Return name and company info for caller from bbdb lookup."
  (interactive)
  (let* (name rec caller)
    (setq name (completing-read "Who is calling? "
                                bbdb-hashtable
                                'bbdb-completion-predicate
                                'confirm))
    (when (> (length name) 0)
      ; Something was supplied - look it up in bbdb
      (setq rec
            (or (first
                 (or (bbdb-search (bbdb-records) name nil nil)
                     (bbdb-search (bbdb-records) nil name nil)))
                name)))

    ; Build the bbdb link if we have a bbdb record, otherwise just return the name
    (setq caller (cond ((and rec (vectorp rec))
                        (let ((name (bbdb-record-name rec))
                              (company (first (bbdb-record-organization rec))))
                          (concat "[[bbdb:"
                                  name "]["
                                  name "]]"
                                  (when company
                                    (concat " - " company)))))
                       (rec)
                       (t "NameOfCaller")))
    (insert caller)))

(setq org-agenda-span 'day)

(setq org-stuck-projects (quote ("" nil nil "")))

;; Helper predicates

(defun pj/is-project-p ()
  "Any task with a todo keyword subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun pj/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (pj/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun pj/is-task-p ()
  "Any task with a todo keyword and no subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun pj/is-subproject-p ()
  "Any task which is a subtask of another project."
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

;; (defun pj/list-sublevels-for-projects-indented ()
;;   "List all subtasks when restricted to a subtree.
;; This is normally used by skipping functions where this variable is already local to the agenda."
;;   (if (marker-buffer org-agenda-restrict-begin)
;;       (setq org-tags-match-list-sublevels 'indented)
;;     (setq org-tags-match-list-sublevels nil))
;;   nil)

;; (defun pj/list-sublevels-for-projects ()
;;   "List all sub-levels for projects.
;; Set org-tags-match-list-sublevels so when restricted to a subtree
;; we list all subtasks.
;; This is normally used by skipping functions where this variable is
;; already local to the agenda."
;;   (if (marker-buffer org-agenda-restrict-begin)
;;       (setq org-tags-match-list-sublevels t)
;;     (setq org-tags-match-list-sublevels nil))
;;   nil)

(defvar pj/hide-scheduled-and-waiting-next-tasks t)

(defun pj/toggle-next-task-display ()
  "Toggle the visibility of waiting and scheduled next tasks."
  (interactive)
  (setq pj/hide-scheduled-and-waiting-next-tasks (not pj/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if pj/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

;; (defun pj/skip-stuck-projects ()
;;   "Skip trees that are not stuck projects."
;;   (save-restriction
;;     (widen)
;;     (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
;;       (if (pj/is-project-p)
;;           (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
;;                  (has-next ))
;;             (save-excursion
;;               (forward-line 1)
;;               (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
;;                 (unless (member "WAITING" (org-get-tags))
;;                   (setq has-next t))))
;;             (if has-next
;;                 nil
;;               next-headline)) ; a stuck project, has subtasks but no next task
;;         nil))))

(defun pj/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects."
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (pj/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun pj/skip-non-projects ()
  "Skip trees that are not projects."
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (pj/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((pj/is-project-p)
            nil)
           ((and (pj/is-project-subtree-p) (not (pj/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun pj/skip-project-trees-and-habits ()
  "Skip trees that are projects."
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((pj/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun pj/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and pj/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags)))
        next-headline)
       ((pj/is-project-p)
        next-headline)
       ((and (pj/is-task-p) (not (pj/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

;; (defun pj/skip-project-tasks-maybe ()
;;   "Show tasks related to the current restriction.
;; When restricted to a project, skip project and sub project tasks, habits,
;; NEXT tasks, and loose tasks.  When not restricted, skip project and sub-project
;; tasks, habits, and project related tasks."
;;   (save-restriction
;;     (widen)
;;     (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
;;            (next-headline (save-excursion (or (outline-next-heading) (point-max))))
;;            (limit-to-project (marker-buffer org-agenda-restrict-begin)))
;;       (cond
;;        ((pj/is-project-p)
;;         next-headline)
;;        ((org-is-habit-p)
;;         subtree-end)
;;        ((and (not limit-to-project)
;;              (pj/is-project-subtree-p))
;;         subtree-end)
;;        ((and limit-to-project
;;              (pj/is-project-subtree-p)
;;              (member (org-get-todo-state) (list "NEXT")))
;;         subtree-end)
;;        (t
;;         nil)))))

(defun pj/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((pj/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((pj/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun pj/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((pj/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (pj/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (pj/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun pj/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits."
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((pj/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

;; (defun pj/skip-non-subprojects ()
;;   "Skip trees that are not projects."
;;   (let ((next-headline (save-excursion (outline-next-heading))))
;;     (if (pj/is-subproject-p)
;;         nil
;;       next-headline)))

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

(defun pj/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving."
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-number (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))

(setq org-list-allow-alphabetical t)

;; Explicitly load required exporters
(require 'ox-html)
(require 'ox-latex)
(require 'ox-ascii)

(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

; Make babel results blocks lowercase
(setq org-babel-results-keyword "results")

(defun bh/display-inline-images ()
  "Display inline images."
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (R . t)
         (python . t)
         (ruby . t)
         (gnuplot . t)
         (clojure . t)
         (shell . t)
         (ledger . t)
         (org . t)
         (latex . t))))

; Do not prompt to confirm evaluation
; This may be dangerous - make sure you understand the consequences
; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)

;; Don't enable this because it breaks access to emacs from my Android phone
(setq org-startup-with-inline-images nil)

;; ;; (defvar org-export-docbook-xsl-fo-proc-command)
;; ;; (defvar org-export-docbook-xslt-proc-command)

;; ;; ; experimenting with docbook exports - not finished
;; ;; (setq org-export-docbook-xsl-fo-proc-command "fop %s %s")
;; ;; (setq org-export-docbook-xslt-proc-command "xsltproc --output %s /usr/share/xml/docbook/stylesheet/nwalsh/fo/docbook.xsl %s")
;; ;
;; ; Inline images in HTML instead of producting links to the image
;; (setq org-html-inline-images t)
;; ; Do not use sub or superscripts - I currently don't need this functionality in my documents
;; (setq org-export-with-sub-superscripts nil)
;; ;; Use org.css from the norang website for export document stylesheets
;; (setq org-html-head-extra "<link rel=\"stylesheet\" href=\"http://doc.norang.ca/org.css\" type=\"text/css\" />")
;; (setq org-html-head-include-default-style nil)

;; ;; Do not generate internal css formatting for HTML exports
;; (defvar *pj/org-export-htmlize-output-type* (quote css))
;; ; Export with LaTeX fragments
;; (setq org-export-with-LaTeX-fragments t)
;; ; Increase default number of headings to export
;; (setq org-export-headline-levels 6)

;; ; List of projects
;; ; norang       - http://www.norang.ca/
;; ; doc          - http://doc.norang.ca/
;; ; org-mode-doc - http://doc.norang.ca/org-mode.html and associated files
;; ; org          - miscellaneous todo lists for publishing
;; (setq org-publish-project-alist
;;       ;
;;       ; http://www.norang.ca/  (norang website)
;;       ; norang-org are the org-files that generate the content
;;       ; norang-extra are images and css files that need to be included
;;       ; norang is the top-level project that gets published
;;       (quote (("norang-org"
;;                :base-directory "~/git/www.norang.ca"
;;                :publishing-directory "/ssh:www-data@www:~/www.norang.ca/htdocs"
;;                :recursive t
;;                :table-of-contents nil
;;                :base-extension "org"
;;                :publishing-function org-html-publish-to-html
;;                :style-include-default nil
;;                :section-numbers nil
;;                :table-of-contents nil
;;                :html-head "<link rel=\"stylesheet\" href=\"norang.css\" type=\"text/css\" />"
;;                :author-info nil
;;                :creator-info nil)
;;               ("norang-extra"
;;                :base-directory "~/git/www.norang.ca/"
;;                :publishing-directory "/ssh:www-data@www:~/www.norang.ca/htdocs"
;;                :base-extension "css\\|pdf\\|png\\|jpg\\|gif"
;;                :publishing-function org-publish-attachment
;;                :recursive t
;;                :author nil)
;;               ("norang"
;;                :components ("norang-org" "norang-extra"))
;;               ;
;;               ; http://doc.norang.ca/  (norang website)
;;               ; doc-org are the org-files that generate the content
;;               ; doc-extra are images and css files that need to be included
;;               ; doc is the top-level project that gets published
;;               ("doc-org"
;;                :base-directory "~/git/doc.norang.ca/"
;;                :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs"
;;                :recursive nil
;;                :section-numbers nil
;;                :table-of-contents nil
;;                :base-extension "org"
;;                :publishing-function (org-html-publish-to-html org-org-publish-to-org)
;;                :style-include-default nil
;;                :html-head "<link rel=\"stylesheet\" href=\"/org.css\" type=\"text/css\" />"
;;                :author-info nil
;;                :creator-info nil)
;;               ("doc-extra"
;;                :base-directory "~/git/doc.norang.ca/"
;;                :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs"
;;                :base-extension "css\\|pdf\\|png\\|jpg\\|gif"
;;                :publishing-function org-publish-attachment
;;                :recursive nil
;;                :author nil)
;;               ("doc"
;;                :components ("doc-org" "doc-extra"))
;;               ("doc-private-org"
;;                :base-directory "~/git/doc.norang.ca/private"
;;                :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs/private"
;;                :recursive nil
;;                :section-numbers nil
;;                :table-of-contents nil
;;                :base-extension "org"
;;                :publishing-function (org-html-publish-to-html org-org-publish-to-org)
;;                :style-include-default nil
;;                :html-head "<link rel=\"stylesheet\" href=\"/org.css\" type=\"text/css\" />"
;;                :auto-sitemap t
;;                :sitemap-filename "index.html"
;;                :sitemap-title "Norang Private Documents"
;;                :sitemap-style "tree"
;;                :author-info nil
;;                :creator-info nil)
;;               ("doc-private-extra"
;;                :base-directory "~/git/doc.norang.ca/private"
;;                :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs/private"
;;                :base-extension "css\\|pdf\\|png\\|jpg\\|gif"
;;                :publishing-function org-publish-attachment
;;                :recursive nil
;;                :author nil)
;;               ("doc-private"
;;                :components ("doc-private-org" "doc-private-extra"))
;;               ;
;;               ; Miscellaneous pages for other websites
;;               ; org are the org-files that generate the content
;;               ("org-org"
;;                :base-directory "~/git/org/"
;;                :publishing-directory "/ssh:www-data@www:~/org"
;;                :recursive t
;;                :section-numbers nil
;;                :table-of-contents nil
;;                :base-extension "org"
;;                :publishing-function org-html-publish-to-html
;;                :style-include-default nil
;;                :html-head "<link rel=\"stylesheet\" href=\"/org.css\" type=\"text/css\" />"
;;                :author-info nil
;;                :creator-info nil)
;;               ;
;;               ; http://doc.norang.ca/  (norang website)
;;               ; org-mode-doc-org this document
;;               ; org-mode-doc-extra are images and css files that need to be included
;;               ; org-mode-doc is the top-level project that gets published
;;               ; This uses the same target directory as the 'doc' project
;;               ("org-mode-doc-org"
;;                :base-directory "~/git/org-mode-doc/"
;;                :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs"
;;                :recursive t
;;                :section-numbers nil
;;                :table-of-contents nil
;;                :base-extension "org"
;;                :publishing-function (org-html-publish-to-html)
;;                :plain-source t
;;                :htmlized-source t
;;                :style-include-default nil
;;                :html-head "<link rel=\"stylesheet\" href=\"/org.css\" type=\"text/css\" />"
;;                :author-info nil
;;                :creator-info nil)
;;               ("org-mode-doc-extra"
;;                :base-directory "~/git/org-mode-doc/"
;;                :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs"
;;                :base-extension "css\\|pdf\\|png\\|jpg\\|gif\\|org"
;;                :publishing-function org-publish-attachment
;;                :recursive t
;;                :author nil)
;;               ("org-mode-doc"
;;                :components ("org-mode-doc-org" "org-mode-doc-extra"))
;;               ;
;;               ; http://doc.norang.ca/  (norang website)
;;               ; org-mode-doc-org this document
;;               ; org-mode-doc-extra are images and css files that need to be included
;;               ; org-mode-doc is the top-level project that gets published
;;               ; This uses the same target directory as the 'doc' project
;;               ("tmp-org"
;;                :base-directory "/tmp/publish/"
;;                :publishing-directory "/ssh:www-data@www:~/www.norang.ca/htdocs/tmp"
;;                :recursive t
;;                :section-numbers nil
;;                :table-of-contents nil
;;                :base-extension "org"
;;                :publishing-function (org-html-publish-to-html org-org-publish-to-org)
;;                :html-head "<link rel=\"stylesheet\" href=\"http://doc.norang.ca/org.css\" type=\"text/css\" />"
;;                :plain-source t
;;                :htmlized-source t
;;                :style-include-default nil
;;                :auto-sitemap t
;;                :sitemap-filename "index.html"
;;                :sitemap-title "Test Publishing Area"
;;                :sitemap-style "tree"
;;                :author-info t
;;                :creator-info t)
;;               ("tmp-extra"
;;                :base-directory "/tmp/publish/"
;;                :publishing-directory "/ssh:www-data@www:~/www.norang.ca/htdocs/tmp"
;;                :base-extension "css\\|pdf\\|png\\|jpg\\|gif"
;;                :publishing-function org-publish-attachment
;;                :recursive t
;;                :author nil)
;;               ("tmp"
;;                :components ("tmp-org" "tmp-extra")))))

; I'm lazy and don't want to remember the name of the project to publish when I modify
; a file that is part of a project.  So this function saves the file, and publishes
; the project that includes this file
;
; It's bound to C-S-F12 so I just edit and hit C-S-F12 when I'm done and move on to the next thing
(defun bh/save-then-publish (&optional force)
  (interactive "P")
  (save-buffer)
  (org-save-all-org-buffers)
  (let ((org-html-head-extra)
        (org-html-validation-link "<a href=\"http://validator.w3.org/check?uri=referer\">Validate XHTML 1.0</a>"))
    (org-publish-current-project force)))

(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)

(setq org-latex-listings t)

(setq org-html-xml-declaration (quote (("html" . "")
                                       ("was-html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
                                       ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))))

(setq org-export-allow-BIND t)

; Erase all reminders and rebuilt reminders for today from the agenda
(defun pj/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'pj/org-agenda-to-appt 'append)

; This is at the end of my .emacs - so appointments are set up when Emacs starts
(pj/org-agenda-to-appt)

; Activate appointments so we get notifications
(appt-activate t)

; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'pj/org-agenda-to-appt)

;; Enable abbrev-mode
(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

(global-set-key (kbd "<f5>") 'pj/org-todo)

(defun pj/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (pj/narrow-to-org-subtree)
        (org-show-todo-tree nil))
    (pj/narrow-to-org-subtree)
    (org-show-todo-tree nil)))

(global-set-key (kbd "<S-f5>") 'pj/widen)

(defun pj/widen ()
  "Widen the view."
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-agenda-remove-restriction-lock)
        (when org-agenda-sticky
          (org-agenda-redo)))
    (widen)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "W"
                                  (lambda () (interactive)
                                    (setq pj/hide-scheduled-and-waiting-next-tasks t)
                                    (pj/widen))))
          'append)

(defun pj/restrict-to-file-or-follow (arg)
  "Set agenda restriction to file or with argument invoke follow mode (ARG).
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
  (interactive "p")
  (if (equal arg 4)
      (org-agenda-follow-mode)
    (widen)
    (bh/set-agenda-restriction-lock 4)
    (org-agenda-redo)
    (goto-char (point-min))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "F" 'pj/restrict-to-file-or-follow))
          'append)

(defun pj/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))

(defun pj/narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (org-get-at-bol 'org-hd-marker)
          (pj/narrow-to-org-subtree))
        (when org-agenda-sticky
          (org-agenda-redo)))
    (pj/narrow-to-org-subtree)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "N" 'pj/narrow-to-subtree))
          'append)

(defun pj/narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (pj/narrow-to-org-subtree)))

(defun pj/get-pom-from-agenda-restriction-or-point ()
  (or (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
      (org-get-at-bol 'org-hd-marker)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

(defun pj/narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (pj/get-pom-from-agenda-restriction-or-point)
          (pj/narrow-up-one-org-level))
        (org-agenda-redo))
    (bh/narrow-up-one-org-level)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "U" 'pj/narrow-up-one-level))
          'append)

(defun pj/narrow-to-org-project ()
  (widen)
  (save-excursion
    (pj/find-project-task)
    (pj/narrow-to-org-subtree)))

(defun pj/narrow-to-project ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (pj/get-pom-from-agenda-restriction-or-point)
          (pj/narrow-to-org-project)
          (save-excursion
            (pj/find-project-task)
            (org-agenda-set-restriction-lock)))
        (org-agenda-redo)
        (goto-char (point-min)))
    (pj/narrow-to-org-project)
    (save-restriction
      (org-agenda-set-restriction-lock))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "P" 'pj/narrow-to-project))
          'append)

(defvar pj/project-list nil)

(defun pj/view-next-project ()
  "View next project."
  (interactive)
  (let (num-project-left current-project)
    (unless (marker-position org-agenda-restrict-begin)
      (goto-char (point-min))
      ; Clear all of the existing markers on the list
      (while pj/project-list
        (set-marker (pop pj/project-list) nil))
      (re-search-forward "Tasks to Refile")
      (forward-visible-line 1))

    ; Build a new project marker list
    (unless pj/project-list
      (while (< (point) (point-max))
        (while (and (< (point) (point-max))
                    (or (not (org-get-at-bol 'org-hd-marker))
                        (org-with-point-at (org-get-at-bol 'org-hd-marker)
                          (or (not (pj/is-project-p))
                              (pj/is-project-subtree-p)))))
          (forward-visible-line 1))
        (when (< (point) (point-max))
          (add-to-list 'pj/project-list (copy-marker (org-get-at-bol 'org-hd-marker)) 'append))
        (forward-visible-line 1)))

    ; Pop off the first marker on the list and display
    (setq current-project (pop pj/project-list))
    (when current-project
      (org-with-point-at current-project
        (setq pj/hide-scheduled-and-waiting-next-tasks nil)
        (pj/narrow-to-project))
      ; Remove the marker
      (setq current-project nil)
      (org-agenda-redo)
      (goto-char(point-min))
      (defvar num-projects-left)
      (setq num-projects-left (length pj/project-list))
      (if (> num-projects-left 0)
          (message "%s projects left to view" num-projects-left)
        (goto-char(point))
        (setq pj/hide-scheduled-and-waiting-next-tasks t)
        (error "All projects viewed")))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "V" 'pj/view-next-project))
          'append)

;; Seemingly not needed...:
;; (setq org-show-entry-below (quote ((default))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'pj/set-agenda-restriction-lock))
          'append)

(defun pj/set-agenda-restriction-lock (arg)
  "Set restriction lock to current task subtree or file if prefix (ARG) is specified."
  (interactive "p")
  (let* ((pom (pj/get-pom-from-agenda-restriction-or-point))
         (tags (org-with-point-at pom (org-get-tags))))
    (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
      (save-restriction
        (cond
         ((and (equal major-mode 'org-agenda-mode) pom)
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))
          (org-agenda-redo))
         ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
          (org-agenda-set-restriction-lock 'file))
         (pom
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))))))))


;; Limit restriction lock highlighting to the headline only
(defvar org-agenda-restriction-lock-highlight-subtree)
(setq org-agenda-restriction-lock-highlight-subtree nil)

;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)

;; Keep tasks with dates on the global todo lists
(setq org-agenda-todo-ignore-with-date nil)

;; Keep tasks with deadlines on the global todo lists
(setq org-agenda-todo-ignore-deadlines nil)

;; Keep tasks with scheduled dates on the global todo lists
(setq org-agenda-todo-ignore-scheduled nil)

;; Keep tasks with timestamps on the global todo lists
(setq org-agenda-todo-ignore-timestamp nil)

;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)

;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)

;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)

(setq org-agenda-include-diary nil)
(setq org-agenda-diary-file (concat (file-name-as-directory (car org-agenda-files)) "diary.org"))

(setq org-agenda-insert-diary-extract-time t)

;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))

;; Show all future entries for repeating tasks
(setq org-agenda-show-future-repeats t)

;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

;; Sorting order for tasks on the agenda
(setq org-agenda-sorting-strategy
      (quote ((agenda habit-down time-up user-defined-up effort-up category-keep)
              (todo category-up effort-up)
              (tags category-up effort-up)
              (search category-up))))

;; Start the weekly agenda on Monday
(setq org-agenda-start-on-weekday 1)

;; Following modified for latest verison of org
;; Enable display of the time grid so we can see the marker for the current time
(setq org-agenda-time-grid (quote ((daily today remove-match)
                                   (0900 1100 1300 1500 1700)
                                   "......"
                                   "----------------")))

;; Original version:
;; (setq org-agenda-time-grid (quote ((daily today remove-match)
;;                                    #("----------------" 0 16 (org-heading t))
;;                                    (0900 1100 1300 1500 1700))))

;; Display tags farther right
(setq org-agenda-tags-column -102)

;;
;; Agenda sorting functions
;;
(setq org-agenda-cmp-user-defined 'pj/agenda-sort)

(defmacro pj/agenda-sort-test (fn a b)
  "Test for agenda sort (FN A B)."
  `(cond
    ; if both match leave them unsorted
    ((and (apply ,fn (list ,a))
          (apply ,fn (list ,b)))
     (setq result nil))
    ; if a matches put a first
    ((apply ,fn (list ,a))
     (setq result -1))
    ; otherwise if b matches put b first
    ((apply ,fn (list ,b))
     (setq result 1))
    ; if none match leave them unsorted
    (t nil)))

(defmacro pj/agenda-sort-test-num (fn compfn a b)
  "Agenda sorting of A and B with COMPFN comparison fn, FN helper function."
  `(cond
    ((apply ,fn (list ,a))
     (setq num-a (string-to-number (match-string 1 ,a)))
     (if (apply ,fn (list ,b))
         (progn
           (setq num-b (string-to-number (match-string 1 ,b)))
           (setq result (if (apply ,compfn (list num-a num-b))
                            -1
                          1)))
       (setq result -1)))
    ((apply ,fn (list ,b))
     (setq result 1))
    (t nil)))

(defun pj/agenda-sort (a b)
  "Sorting strategy for agenda items (A B).
Late deadlines first, then scheduled, then non-late deadlines"
  (let (result num-a num-b)
    (cond
     ; time specific items are already sorted first by org-agenda-sorting-strategy

     ; non-deadline and non-scheduled items next
     ((pj/agenda-sort-test 'pj/not-scheduled-or-deadline-p a b))

     ; deadlines for today next
     ((pj/agenda-sort-test 'pj/due-deadline-p a b))

     ; late deadlines next
     ((pj/agenda-sort-test-num 'pj/late-deadline-p '> a b))

     ; scheduled items for today next
     ((pj/agenda-sort-test 'pj/scheduled-today-p a b))

     ; late scheduled items next
     ((pj/agenda-sort-test-num 'pj/scheduled-late-p '> a b))

     ; pending deadlines last
     ((pj/agenda-sort-test-num 'pj/pending-deadline-p '< a b))

     ; finally default to unsorted
     (t (setq result nil)))
    result))

;; TODO: Understand how this can be changed to positive
;;       result directly, rather than t being the negative.
;;       Key point: Use of this function in the macro above.

(defun pj/not-scheduled-or-deadline-p (date-str)
  "Task with DATE-STR is not a due deadline or scheduled."
  (and (not (pj/deadline-p date-str))
       (not (pj/scheduled-p date-str))))

(defun pj/due-deadline-p (date-str)
  "DATE-STR - due deadline?"
  (string-match "Deadline:" date-str))

(defun pj/late-deadline-p (date-str)
  "DATE-STR - late deadline?"
  (string-match "\\([0-9]*\\) d\. ago:" date-str))

(defun pj/pending-deadline-p (date-str)
  "DATE-STR - Pending deadline?"
  (string-match "In \\([^-]*\\)d\.:" date-str))

(defun pj/deadline-p (date-str)
  "DATE-STR - Deadline?"
  (or (pj/due-deadline-p date-str)
      (pj/late-deadline-p date-str)
      (pj/pending-deadline-p date-str)))

(defun pj/scheduled-p (date-str)
  "DATE-STR - Scheduled?"
  (or (pj/scheduled-today-p date-str)
      (pj/scheduled-late-p date-str)))

(defun pj/scheduled-today-p (date-str)
  "DATE-STR - Scheduled today?"
  (string-match "Scheduled:" date-str))

(defun pj/scheduled-late-p (date-str)
  "DATE-STR - Scheduled but late?"
  (string-match "Sched\.\\(.*\\)x:" date-str))

;; Use sticky agenda's so they persist
(setq org-agenda-sticky t)



(setq org-enforce-todo-dependencies t)

(setq org-hide-leading-stars nil)

(setq org-startup-indented t)

(setq org-cycle-separator-lines 0)

(setq org-blank-before-new-entry (quote ((heading)
                                         (plain-list-item . auto))))

(setq org-insert-heading-respect-content nil)

(setq org-reverse-note-order nil)

(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)
(setq org-show-siblings (quote ((default))))

(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)

(setq org-id-method (quote uuidgen))

(setq org-deadline-warning-days 30)

(setq org-table-export-default-format "orgtbl-to-csv")

(setq org-link-frame-setup (quote ((vm . vm-visit-folder)
                                   (gnus . org-gnus-no-new-news)
                                   (file . find-file))))

; Use the current window for C-c ' source editing
(setq org-src-window-setup 'current-window)

(setq org-log-done (quote time))
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)

(setq org-clock-sound "/usr/local/lib/tngchime.wav")

; Enable habit tracking (and a bunch of other modules)
(setq org-modules (quote (org-bbdb
                          org-bibtex
                          org-crypt
                          org-gnus
                          org-id
                          org-info
                          org-jsinfo
                          org-habit
                          org-inlinetask
                          org-irc
                          org-mew
                          org-mhe
                          org-protocol
                          org-rmail
                          org-vm
                          org-wl
                          org-w3m)))

; position the habit graph on the agenda to the right of the default
(setq org-habit-graph-column 50)

(run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))

(global-auto-revert-mode t)


; Encrypt all entries before saving
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
; GPG key to use for encryption
(setq org-crypt-key "F0B66B40")

(setq org-crypt-disable-auto-save nil)

(setq org-use-speed-commands t)
(setq org-speed-commands-user (quote (("0" . ignore)
                                      ("1" . ignore)
                                      ("2" . ignore)
                                      ("3" . ignore)
                                      ("4" . ignore)
                                      ("5" . ignore)
                                      ("6" . ignore)
                                      ("7" . ignore)
                                      ("8" . ignore)
                                      ("9" . ignore)

                                      ("a" . ignore)
                                      ("d" . ignore)
                                      ("i" progn
                                       (forward-char 1)
                                       (call-interactively 'org-insert-heading-respect-content))
                                      ("k" . org-kill-note-or-show-branches)
                                      ("l" . ignore)
                                      ("m" . ignore)
                                      ("q" . pj/show-org-agenda)
                                      ("r" . ignore)
                                      ("s" . org-save-all-org-buffers)
                                      ("w" . org-refile)
                                      ("x" . ignore)
                                      ("y" . ignore)
                                      ("z" . org-add-note)

                                      ("A" . ignore)
                                      ("B" . ignore)
                                      ("E" . ignore)
                                      ("F" . pj/restrict-to-file-or-follow)
                                      ("G" . ignore)
                                      ("H" . ignore)
                                      ("J" . org-clock-goto)
                                      ("K" . ignore)
                                      ("L" . ignore)
                                      ("M" . ignore)
                                      ("N" . pj/narrow-to-org-subtree)
                                      ("P" . pj/narrow-to-org-project)
                                      ("Q" . ignore)
                                      ("R" . ignore)
                                      ("S" . ignore)
                                      ("T" . pj/org-todo)
                                      ("U" . pj/narrow-up-one-org-level)
                                      ("V" . ignore)
                                      ("W" . pj/widen)
                                      ("X" . ignore)
                                      ("Y" . ignore)
                                      ("Z" . ignore))))

(defun pj/show-org-agenda ()
  "Show the agenda."
  (interactive)
  (if org-agenda-sticky
      (switch-to-buffer "*Org Agenda( )*")
    (switch-to-buffer "*Org Agenda*"))
  (delete-other-windows))



(setq require-final-newline t)

(defvar *pj/insert-inactive-timestamp* t)

(defun pj/toggle-insert-inactive-timestamp ()
  "Toggle insert inactive timestamp preference and modify header message."
  (interactive)
  (setq *pj/insert-inactive-timestamp* (not *pj/insert-inactive-timestamp*))
  (message "Heading timestamps are %s" (if *pj/insert-inactive-timestamp* "ON" "OFF")))

(defun pj/insert-inactive-timestamp ()
  "Insert timestamp."
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun pj/insert-heading-inactive-timestamp ()
  "Insert heading inactive timestamp."
  (save-excursion
    (when bh/insert-inactive-timestamp
      (org-return)
      (org-cycle)
      (bh/insert-inactive-timestamp))))

(add-hook 'org-insert-heading-hook 'pj/insert-heading-inactive-timestamp 'append)

(setq org-export-with-timestamps nil)

(setq org-return-follows-link t)

;; (defun pj/prepare-meeting-notes ()
;;   "Prepare meeting notes for email.
;; Take selected region and convert tabs to spaces, mark TODOs with leading >>>, and copy to kill ring for pasting"
;;   (interactive)
;;   (let (prefix)
;;     (save-excursion
;;       (save-restriction
;;         (narrow-to-region (region-beginning) (region-end))
;;         (untabify (point-min) (point-max))
;;         (goto-char (point-min))
;;         (while (re-search-forward "^\\( *-\\\) \\(TODO\\|DONE\\): " (point-max) t)
;;           (replace-match (concat (make-string (length (match-string 1)) ?>) " " (match-string 2) ": ")))
;;         (goto-char (point-min))
;;         (kill-ring-save (point-min) (point-max))))))

(setq org-remove-highlights-with-change t)

(add-to-list 'Info-default-directory-list *pj/info-default-directory-list*)

(setq org-read-date-prefer-future 'time)

(setq org-list-demote-modify-bullet (quote (("+" . "-")
                                            ("*" . "-")
                                            ("1." . "-")
                                            ("1)" . "-")
                                            ("A)" . "-")
                                            ("B)" . "-")
                                            ("a)" . "-")
                                            ("b)" . "-")
                                            ("A." . "-")
                                            ("B." . "-")
                                            ("a." . "-")
                                            ("b." . "-"))))

(setq org-tags-match-list-sublevels t)

(setq org-agenda-persistent-filter t)

(setq org-link-mailto-program (quote (compose-mail "%a" "%s")))

;; Bookmark handling
(global-set-key (kbd "<C-f6>") '(lambda () (interactive) (bookmark-set "SAVED")))
(global-set-key (kbd "<f6>") '(lambda () (interactive) (bookmark-jump "SAVED")))

(use-package org-mime
  :ensure t)

(setq org-agenda-skip-additional-timestamps-same-entry t)

(setq org-table-use-standard-references (quote from))

(setq org-file-apps (quote ((auto-mode . emacs)
                            ("\\.mm\\'" . system)
                            ("\\.x?html?\\'" . system)
                            ("\\.pdf\\'" . system))))

; Overwrite the current window with the agenda
(setq org-agenda-window-setup 'current-window)

(setq org-clone-delete-id t)

(setq org-cycle-include-plain-lists t)

(setq org-src-fontify-natively t)

(setq org-structure-template-alist
      (quote (("s" . "src")
              ("e" . "example")
              ("q" . "quote")
              ("v" . "verse")
              ("c" . "center")
              ("l" . "latex")
              ;("L" "#+latex: " "<literal style=\"latex\">?</literal>")
              ("h" . "export html")
              ("H" . "html")
              ("a" . "export ascii")
              ("A" . "ascii")
              ;("i" "#+index: ?" "#+index: ?")
              ;("I" "#+include %file ?" "<include file=%file markup=\"?\">")
              )))

(defun pj/mark-next-parent-tasks-todo ()
  "Visit each parent task and change NEXT states to TODO."
  (let ((mystate (or (and (fboundp 'org-state)
                          state)
                     (nth 2 (org-heading-components)))))
    (when mystate
      (save-excursion
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) (list "NEXT"))
            (org-todo "TODO")))))))

(add-hook 'org-after-todo-state-change-hook 'pj/mark-next-parent-tasks-todo 'append)
(add-hook 'org-clock-in-hook 'pj/mark-next-parent-tasks-todo 'append)

(setq org-startup-folded t)

(add-hook 'message-mode-hook 'orgstruct++-mode 'append)
(add-hook 'message-mode-hook 'turn-on-auto-fill 'append)
(add-hook 'message-mode-hook 'bbdb-define-all-aliases 'append)
(add-hook 'message-mode-hook 'orgtbl-mode 'append)
(add-hook 'message-mode-hook 'turn-on-flyspell 'append)
(add-hook 'message-mode-hook
          '(lambda () (setq fill-column 72))
          'append)

;; flyspell mode for spell checking everywhere
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)

;; Disable keys in org-mode
;;    C-c [ 
;;    C-c ]
;;    C-c ;
;;    C-c C-x C-q  cancelling the clock (we never want this)
(add-hook 'org-mode-hook
          '(lambda ()
             ;; Undefine C-c [ and C-c ] since this breaks my
             ;; org-agenda files when directories are include It
             ;; expands the files in the directories individually
             (org-defkey org-mode-map "\C-c[" 'undefined)
             (org-defkey org-mode-map "\C-c]" 'undefined)
             (org-defkey org-mode-map "\C-c;" 'undefined)
             (org-defkey org-mode-map "\C-c\C-x\C-q" 'undefined))
          'append)

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c M-o") 'pj/mail-subtree))
          'append)

;; TODO: This doesn't do anything useful at the moment. Perhaps after
;;       getting mu4e to work it will.
(defun pj/mail-subtree ()
  "Mark and send subtree as email."
  (interactive)
  (org-mark-subtree)
  (org-mime-org-subtree-htmlize))

(setq org-src-preserve-indentation nil)
(setq org-edit-src-content-indentation 0)

(setq org-catch-invisible-edits 'error)

(setq org-export-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

(setq org-emphasis-alist (quote (("*" bold "<b>" "</b>")
                                 ("/" italic "<i>" "</i>")
                                 ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
                                 ("=" org-code "<code>" "</code>" verbatim)
                                 ("~" org-verbatim "<code>" "</code>" verbatim))))

(setq org-hide-emphasis-markers t)
; Match - in lists and replace with centre dot character
(font-lock-add-keywords 'org-mode
                        '(("^ *\\[-] "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(add-hook 'org-mode-hook 'visual-line-mode)


(setq org-use-sub-superscripts nil)

(setq org-odd-levels-only nil)

(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button))))))

(run-at-time "00:59" 3600 'org-save-all-org-buffers)

(provide 'my-org-mode)
;;; my-org-mode.el ends here

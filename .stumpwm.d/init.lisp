;; -*- Mode: lisp; Mode: StumpWM -*-
(in-package :stumpwm)

(add-to-load-path "/home/paul/.stumpwm.d/modules")

(ql:quickload '(:clx-truetype
                :alexandria
                :xembed))  ;; Required by stumptray

;; load Stump contrib modules
(mapc #'load-module
      '("ttf-fonts"
	     "swm-gaps"
        "stumptray"))

(setq *mouse-focus-policy* :click)

;; font settings


;; (xft:cache-fonts)
(defparameter *awesome-font-name* "FontAwesome" "The font name used for Awesome Font")
(defparameter *terminal-command-name* "alacritty")
;; TODO: Make this a configuration record, and amend as required for different hosts
(if (string-equal (machine-instance) "tristan")
    (progn (setf *awesome-font-name* "Font Awesome 5 Free Regular")
           (setf *terminal-command-name* "alacritty"))
    (progn (setf *awesome-font-name* "FontAwesome")
           (setf *terminal-command-name* "urxvt")))

(set-font (list
	        (make-instance 'xft:font
			                 :family "DejaVu Sans Mono"
			                 :subfamily "Bold"
			                 :size 12)
	        (make-instance 'xft:font
			                 :family *awesome-font-name*
			                 :subfamily "Regular"
			                 :size 12)))

(setf *colors*
      '("#ffffff"        ; ^0 ; White
	     "#131220"        ; ^1 ; Dark Blue
	     "#f72f33"        ; ^2 ; Red
	     "#689d6a"        ; ^3 ; Light Green
	     "#62bfef"        ; ^4 ; Light Blue
        "#fabd2f"        ; ^5 ; Yellow / Help map keys
	     "#a644bf"        ; ^6 ; Magenta
	     "#cc4a0e"        ; ^7 ; Brown
	     "#56b6c2"))      ; ^8 ; Cyan  
	
(defparameter *mode-line-bg-color* (nth 1 *colors*))
(defparameter *mode-line-fg-color* (nth 0 *colors*))
(defparameter *msg-bg-color* (nth 1 *colors*))
(defparameter *msg-fg-color* (nth 0 *colors*))
(defparameter *msg-border-color* (nth 2 *colors*))

;;; Changing the prefix key
(set-prefix-key (kbd "s-z"))

(defcommand launcher () ()
  (run-shell-command
   "rofi -show drun"))

;;; Load swank on demand
;; Note: This path will unfortunately change when slime is updated
(load "~/.emacs.d/elpa/slime-20230109.1535/swank-loader.lisp")
(swank-loader:init)

(defparameter *port-number* 4005
  "My default port number for Swank")

(defvar *swank-server-p* nil
  "Keep track of Swank server, only started on request")

(defcommand start-swank () ()
  "Start Swank if not already running"
  (if *swank-server-p*
      (message "Swank server is already active on Port^5 ~a^n" *port-number*)
      (progn
        (swank:create-server :port *port-number*
                             :style swank:*communication-style*
                             :dont-close t)
        (setf *swank-server-p* t)
        (echo-string (current-screen)
                     "Starting swank. M-x slime-connect RET RET, then (in-package stumpwm)."))))

(defcommand stop-swank () ()
  "Stop Swank"
  (swank:stop-server *port-number*)
  (setf *swank-server-p* nil)
  (message "Stopping Swank Server! Closing Port^5 ~a^n." *port-number*))

(define-key *root-map* (kbd "C-s") "start-swank")

;;; Process management
;; Thanks to tomoyuki28jp:
;; https://github.com/tomoyuki28jp/stumpwm/blob/master/.stumpwmrc
(defun ps-exists (ps)
  (let ((f "ps -ef | grep ~S | grep -v -e grep -e stumpish | wc -l"))
    (< 0 (parse-integer (run-shell-command (format nil f ps) t)))))

(defun start-uniq-command-ps (command &key options (background t))
  (unless (ps-exists command)
         (run-shell-command
          (concat command " " options " " (when background "&")))))

;; ---------------------------------------------------------------------

(defcommand nm-applet () ()
  (start-uniq-command-ps "nm-applet" :options "--sm-disable"))

;;; Run commands
(when *initializing* 
  (nm-applet)
  (run-shell-command "emacs --daemon")
  (start-uniq-command-ps "redshift" :options "-l 53:0")
  (start-uniq-command-ps "picom" :options "--experimental-backends")
  (start-uniq-command-ps "nitrogen --restore")
  (start-uniq-command-ps "sleep 5 && volumeicon")
  (start-uniq-command-ps "telegram-desktop -startintray")
  (start-uniq-command-ps "slack")
  (start-uniq-command-ps "dunst")
  (start-uniq-command-ps "nextcloud")
  (start-uniq-command-ps "/usr/bin/gentoo-pipewire-launcher"))
;;(start-emacs)

(defcommand emacsclient () ()
  "Open / raise client window"
  (run-or-raise "emacsclient -n -c -e '(switch-to-buffer nil)'" '(:class "Emacs") nil T)
  (message "GNU-Emacs"))

(defcommand new-emacsclient () ()
  "Open new client window"
  (run-shell-command "emacsclient -n -c -e '(switch-to-buffer nil)'"))

(defcommand browser () ()
  (run-or-raise "firefox" '(:class "Firefox"))) 

(defcommand terminal () ()
  (run-shell-command *terminal-command-name*))

(defcommand mail-client () ()
  (run-or-raise "thunderbird" '(:class "Thunderbird")))

(defcommand lights-on () ()
  (run-shell-command "/home/paul/bin/lights-on"))

(defcommand lights-off () ()
  (run-shell-command "/home/paul/bin/lights-off"))

(defcommand toggle-lights () ()
  (run-shell-command "/home/paul/bin/toggle-lights"))

;;; Modeline
(defvar *swank-ml-status* ""
  "Modeline status for Swank server")

(defun get-swank-status ()
  (if *swank-server-p*
      (setf *swank-ml-status* (format nil "Swank ^3^f1^f0^n Port:^5 ~a^n " *port-number*))
      (setf *swank-ml-status* "")))

(defun ml-fmt-swank-status (ml)
  (declare (ignore ml))
  (get-swank-status))

(add-screen-mode-line-formatter #\S #'ml-fmt-swank-status)

;;; Modeline settings

(setf *mode-line-timeout* 1)
(setf *mode-line-border-width* 0)

(setf *mode-line-background-color* *mode-line-bg-color*)
(setf *mode-line-border-color* *mode-line-bg-color*)
(setf *mode-line-foreground-color* *mode-line-fg-color*)

(setf *time-modeline-string* "^2^f1^f0^n %H:%M")

(defparameter *battery-percent* "")

(defun get-battery-status ()
  (let* ((batgetcap (run-shell-command "cat /sys/class/power_supply/BAT0/capacity | tr -d '\\r\\n'" t)))
    (if (> 0 (length batgetcap)) ;; battery is fitted...
        (setf *battery-percent* (format nil "^4^f1^f0^n ~a% " batgetcap))
        (setf *battery-percent* ""))))

(defun battery-percentage (ml)
  (declare (ignore ml))
  *battery-percent*)
  
(run-with-timer 0 10 #'get-battery-status)

(add-screen-mode-line-formatter #\B #'battery-percentage)

(setf *screen-mode-line-format*
      (list "^5[%g]^n "   ; groups
            "%W"          ; windows
            "^>"          ; right align
            "%S"          ; Swank status
            "%B"          ; Battery status
            "%d"          ; time/date
            "%T"))        ; Space for stumptray

(if (not (head-mode-line (current-head)))
    (toggle-mode-line (current-screen) (current-head)))

(stumptray:stumptray)


(defvar *pj/automation-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "a") "toggle-lights")
    (define-key m (kbd "o") "lights-on")
    (define-key m (kbd "f") "lights-off")
    m))


(dolist
    (binding `(
               ;; Multimedia keys
               (,(kbd "XF86AudioLowerVolume")  "exec amixer set Master 5%-")
               (,(kbd "XF86AudioRaiseVolume")  "exec amixer set Master 5%+")
               ;; F2/F3 for keyboard without Multimedia keys
               (,(kbd "s-F2") "exec amixer set Master 5%-")
               (,(kbd "s-F3") "exec amixer set Master 5%+")
               
               ;; Mute
               (,(kbd "XF86AudioMute")         "exec amixer set Master toggle")
               (,(kbd "XF86AudioMicMute")      "exec amixer set Capture toggle")
               ;; Screen brightness
               (,(kbd "XF86MonBrightnessDown") "exec light -U 10")
               (,(kbd "XF86MonBrightnessUp")   "exec light -A 10")

               ;;...Favorite applications
               (,(kbd "s-e")                   "emacsclient")
               (,(kbd "s-w")                   "browser")
               (,(kbd "s-RET")                 "terminal")
               (,(kbd "s-p")                   "launcher")
               (,(kbd "s-E")                   "mail-client")
               
               (,(kbd "s-SPC")                 "pull-hidden-other")

               ;; Window splits
               (,(kbd "s-S")                   "hsplit")
               (,(kbd "s-s")                   "vsplit")
               (,(kbd "s-R")                   "remove")
               (,(kbd "s-r")                   "iresize")
               
               ;; Frame movement & focus
               (,(kbd "s-Right")               "move-focus right")
               (,(kbd "s-Left")                "move-focus left")
               (,(kbd "s-Up")                  "move-focus up")
               (,(kbd "s-Down")                "move-focus down")
               ;; Vim bindings...
               (,(kbd "s-h")                   "move-focus left")
               (,(kbd "s-j")                   "move-focus down")
               (,(kbd "s-k")                   "move-focus up")
               (,(kbd "s-l")                   "move-focus right")
               ;; Move window to frame
               (,(kbd "s-S-Right")             "move-window right")
               (,(kbd "s-S-Left")              "move-window left")
               (,(kbd "s-S-Up")                "move-window up")
               (,(kbd "s-S-Down")              "move-window down")
               ;; Vim bindings
               (,(kbd "s-H")                   "move-window left")
               (,(kbd "s-J")                   "move-window down")
               (,(kbd "s-K")                   "move-window up")
               (,(kbd "s-L")                   "move-window right")
               (,(kbd "s-q")                     "quit-confirm")
               (,(kbd "s-a")                  ,*pj/automation-keymap*)))
  (apply (alexandria:curry #'define-key *top-map*) binding))

;;(define-key *top-map* (kbd "s-e") "emacsclient" )



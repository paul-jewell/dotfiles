;;; helper-functions --- general helper functions for emacs.
;;; commentary:
;;;   - Functions to add ID properties to all tasks in an org buffer.

;;; Code:

(require 'org) ;; "~/Nextcloud/git/org-mode/lisp/org.el"
(require 'my-org-mode "~/.emacs.d/lisp/my-org-mode.el")

(defun pj/get-property-id ()
  "Return the property ID, or nil of not present."
  (interactive)
  (org-element-property :ID (org-element-at-point)))

(defun pj/put-property-id ()
  "Add a UUID ID property for the current task."
  (interactive)
  (org-entry-put (point) "ID" (pj/uuidgen)))
  
(defun pj/add-property-id ()
  "Add UUID ID property, of not already present."
  (interactive)
  (unless (pj/get-property-id)
    (pj/put-property-id)))

(defun pj/add-property-id-to-buffer ()
  "Map all org entries in buffer - add property id if missing."
  (interactive)
  (org-map-entries #'pj/add-property-id))

(provide 'helper-functions)
;;; helper-functions.el ends here

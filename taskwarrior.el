;;; taskwarrior.el --- An interactive taskwarrior interface -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Patrick Winter

;; Author: Patrick Winter <patrickwinter@posteo.ch>
;; Keywords: Tools
;; Url: https://gitlab/winpat/taskwarrior.el
;; Package-requires: ((emacs "26.3"))
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The following todo exist:
;; TODO: Implement modeline indicator for deadline and entries
;; TODO: Update buffer if command modifies the state
;; TODO: Extract "1-1000" id filter into variable
;; TODO: Figure out the difference between assoc and assoc-string
;;       (and why alist-get does not work with strings)
;; TODO: Restore position after taskwarrior-update-buffer is called

;;; Code:

(require 'json)
(require 'dash)
(require 'transient)

(defgroup taskwarrior nil "An emacs frontend to taskwarrior.")

(defvar taskwarrior-buffer-name "*taskwarrior*")

(defvar taskwarrior-active-filter nil "Currently active filter.")

(defvar taskwarrior-active-profile nil "Name of the currently active profile.")

(defvar taskwarrior-description 'taskwarrior-description
  "Taskwarrior mode face used for tasks with a priority of C.")

(defvar taskwarrior-profile-alist nil
  "A list of named filters in the form an associative list.")

(defface taskwarrior-priority-high-face
  '((((min-colors 88) (class color))
     (:foreground "red1"))
    (((class color))
     (:foreground "red"))
    (t (:weight bold :underline t)))
  "Face used for H priority label."
  :group 'taskwarrior-faces)

(defvar taskwarrior-priority-high-face  'taskwarrior-priority-high-face
  "Face name to use for high priority label.")

(defface taskwarrior-priority-medium-face
  '((((min-colors 88) (class color))
     (:foreground "orange1"))
    (((class color))
     (:foreground "orange"))
    (t (:weight bold :underline t)))
  "Face used for M priority label."
  :group 'taskwarrior-faces)

(defvar taskwarrior-priority-medium-face  'taskwarrior-priority-medium-face
  "Face name to use for medium priority label.")

(defface taskwarrior-priority-low-face
  '((((min-colors 88) (class color))
     (:foreground "grey1"))
    (((class color))
     (:foreground "grey"))
    (t (:weight bold :underline t)))
  "Face used for L priority label."
  :group 'taskwarrior-faces)

(defvar taskwarrior-priority-low-face  'taskwarrior-priority-low-face
  "Face name to use for low priority label.")

(setq taskwarrior-highlight-regexps
      `(("^\\*.*$"             . font-lock-variable-name-face)
	("^ [0-9]*"            . font-lock-variable-name-face)
	("([0-9.]*?)"          . font-lock-builtin-face)
	("\\+[a-zA-Z0-9\\-_]+" . font-lock-doc-face)
	("\\[.*\\]"            . font-lock-preprocessor-face)
	("[:space:].*:"        . font-lock-function-name-face)
	(" L "             . taskwarrior-priority-low-face)
	(" M "             . taskwarrior-priority-medium-face)
	(" H "             . taskwarrior-priority-high-face)))

(defvar taskwarrior-mode-map nil "Keymap for `taskwarrior-mode'.")
(progn
  (setq taskwarrior-mode-map (make-sparse-keymap))
  (define-key taskwarrior-mode-map (kbd "a") 'taskwarrior-add)
  (define-key taskwarrior-mode-map (kbd "A") 'taskwarrior-annotate)
  (define-key taskwarrior-mode-map (kbd "d") 'taskwarrior-date)
  (define-key taskwarrior-mode-map (kbd "x") 'taskwarrior-done)
  (define-key taskwarrior-mode-map (kbd "D") 'taskwarrior-delete)
  (define-key taskwarrior-mode-map (kbd "e") 'taskwarrior-edit-description)
  (define-key taskwarrior-mode-map (kbd "q") 'quit-window)
  (define-key taskwarrior-mode-map (kbd "g") 'taskwarrior-update-buffer)
  (define-key taskwarrior-mode-map (kbd "q") 'quit-window)
  (define-key taskwarrior-mode-map (kbd "U") 'taskwarrior-edit-priority)
  (define-key taskwarrior-mode-map (kbd "l") 'taskwarrior-load-profile)
  (define-key taskwarrior-mode-map (kbd "o") 'taskwarrior-open-annotation)
  (define-key taskwarrior-mode-map (kbd "m") 'taskwarrior-mark-task)
  (define-key taskwarrior-mode-map (kbd "u") 'taskwarrior-unmark-task)
  (define-key taskwarrior-mode-map (kbd "r") 'taskwarrior-reset-filter)
  (define-key taskwarrior-mode-map (kbd "f") 'taskwarrior-set-filter)
  (define-key taskwarrior-mode-map (kbd "t") 'taskwarrior-edit-tags)
  (define-key taskwarrior-mode-map (kbd "RET") 'taskwarrior-info)
  (define-key taskwarrior-mode-map (kbd "P") 'taskwarrior-edit-project))

(defun taskwarrior-load-profile (profile)
  "Load a predefined taskwarrior PROFILE."
  (interactive
   (list (completing-read "Profile: " (-map 'car taskwarrior-profile-alist))))
  (let ((filter (cdr (assoc-string profile taskwarrior-profile-alist))))
    (progn
      (setq taskwarrior-active-profile profile)
      (setq taskwarrior-active-filter filter)
      (taskwarrior-update-buffer))))

(defun taskwarrior-unmark-task ()
  "Unmark task."
  (interactive)
  (let ((id (taskwarrior-id-at-point)))
    (if (local-variable-p 'taskwarrior-marks)
	(setq-local taskwarrior-marks (remove id taskwarrior-marks))))
  (progn
    (save-excursion
      (read-only-mode -1)
      (beginning-of-line)
      (delete-forward-char 1)
      (insert " ")
      (read-only-mode 1))
    (next-line)))

(defun taskwarrior-mark-task ()
  "Mark current task."
  (interactive)
  (let ((id (taskwarrior-id-at-point)))
    (if (local-variable-p 'taskwarrior-marks)
	(setq-local taskwarrior-marks (delete-dups (cons id taskwarrior-marks)))
      (setq-local taskwarrior-marks (list id)))
    (progn
      (save-excursion
	(read-only-mode -1)
	(beginning-of-line)
	(delete-forward-char 1)
	(insert "*")
	(read-only-mode 1))
      (next-line))))

(defun taskwarrior-open-annotation ()
  "Open annotation on task."
  (interactive)
  (let* ((id (taskwarrior-id-at-point))
	 (task  (taskwarrior-export-task id))
	 (annotations (-map (lambda (x) (alist-get 'description x)) (taskwarrior-vector-to-list (alist-get 'annotations task))))
	 (choice (completing-read "Tags: " annotations)))
    (org-open-link-from-string choice)))

(defun taskwarrior-info ()
  "Display detailed information about task."
  (interactive)
  (let* ((id (taskwarrior-id-at-point))
	 (buf (get-buffer-create "*taskwarrior info*")))
    (progn
      (switch-to-buffer-other-window buf)
      (erase-buffer)
      (insert (taskwarrior--shell-command "info" id)))))

(defun taskwarrior--parse-created-task-id (output)
  "Extract task id from shell OUTPUT of `task add`."
  (when (string-match "^.*Created task \\([0-9]+\\)\\.*$" output)
    (message (match-string 1 output))))

(defun taskwarrior--parse-org-link (link)
  "Extract 'org-mode' link from LINK."
    (string-match org-bracket-link-regexp link)
    (list
     (match-string 1 link)
     (match-string 3 link)))

(defun taskwarrior-capture (arg)
  "Capture a taskwarrior task with content ARG."
  (interactive "P")
  (let* ((link (car (cdr (taskwarrior--parse-org-link (org-store-link arg)))))
	 (description (read-from-minibuffer "Description: "))
	 (id (taskwarrior--parse-created-task-id
	      (shell-command-to-string (format "task add %s" description)))))
    (shell-command-to-string (format "task %s annotate %s" id link))))

(defun taskwarrior-id-at-point ()
  "Get id of task at point."
  (let ((line (thing-at-point 'line t)))
    (string-match "^  [0-9]*" line)
    (string-trim-left (match-string 0 line))))

(defun taskwarrior-task-at-point ()
  "Get id of task at point."
  (let ((id (taskwarrior-id-at-point)))
    (taskwarrior-export-task id)))

(defun taskwarrior-reset-filter ()
  "Reset the currently set filter."
  (interactive)
  (progn
    (setq-local taskwarrior-active-filter nil)
    (taskwarrior-update-buffer)))

(defun taskwarrior-set-filter ()
  "Set or edit the current filter."
  (interactive)
  (let ((new-filter (read-from-minibuffer "Filter: " taskwarrior-active-filter)))
    (progn
      (setq-local taskwarrior-active-filter new-filter)
      (taskwarrior-update-buffer))))

(defun taskwarrior--shell-command (command &optional filter modifications miscellaneous confirm)
  "Run a taskwarrior COMMAND with specified FILTER MODIFICATIONS MISCELLANEOUS CONFIRM."
  (let* ((confirmation (if confirm (concat "echo " confirm " |") ""))
	 (cmd (format "%s task %s %s %s %s"
		      (or confirmation "")
		      (or filter "")
		      (or command "")
		      (or modifications "")
		      (or miscellaneous ""))))
    (progn
      (message cmd)
      (shell-command-to-string cmd))))

(defun taskwarrior-vector-to-list (vector)
  "Convert a VECTOR to a list."
  (append vector nil))

(defun taskwarrior--concat-tag-list (tags)
  "Concat a list of TAGS in to readable format."
  (mapconcat
   (function (lambda (x) (format "+%s" x)))
   (taskwarrior-vector-to-list tags)
   " "))

(defun taskwarrior-export (&optional filter)
  "Turn task export into the tabulated list entry form filted by FILTER."
  (let ((filter (concat filter " id.not:0")))
    (mapcar
     (lambda (entry)
       (let* ((id          (format "%s" (alist-get 'id entry)))
	      (urgency     (format "%0.2f" (alist-get 'urgency entry)))
	      (priority    (format " %s " (or (alist-get 'priority entry) "")))
	      (annotations (format "%d"  (length (or (alist-get 'annotations entry) '()))))
	      (project     (or (alist-get 'project entry) ""))
	      (tags        (or (taskwarrior--concat-tag-list (alist-get 'tags entry)) ""))
	      (description (format "%s" (alist-get 'description entry))))
	 `(,id [,id ,urgency ,priority ,annotations ,project ,tags ,description])))
     (taskwarrior-vector-to-list
      (json-read-from-string
       (taskwarrior--shell-command "export" filter))))))

(defun taskwarrior-update-buffer ()
  "Update the taskwarrior buffer."
  (interactive)
    (progn
      (setq tabulated-list-entries
	    (if taskwarrior-active-filter
		(taskwarrior-export taskwarrior-active-filter)
	      (taskwarrior-export)))
      (tabulated-list-print t)))

(defun taskwarrior-export-task (id)
  "Export task with ID."
  (let ((task (taskwarrior-vector-to-list
	       (json-read-from-string
		(taskwarrior--shell-command "export" (concat "id:" id))))))
    (if (< (length task) 1)
	(error "Seems like two task have the same id")
      (car task))))

(defun taskwarrior--change-attribute (attribute)
  "Change an ATTRIBUTE of a task."
  (let* ((prefix (format "%s: " (capitalize attribute)))
	 (id (taskwarrior-id-at-point))
	 (task (taskwarrior-export-task id))
	 (old (cdr (assoc-string attribute task)))
	 (new (read-from-minibuffer prefix old))
         (escaped (format "%s:\"%s\"" attribute new)))
    (taskwarrior--mutable-shell-command "modify" id escaped)))

(defun taskwarrior-edit-tags ()
  "Edit tags on task."
  (interactive)
  (let* ((id (taskwarrior-id-at-point))
	 (task (taskwarrior-export-task id))
	 (options (split-string (shell-command-to-string "task _tags") "\n"))
	 (old (taskwarrior-vector-to-list (alist-get 'tags task)))
	 (current-tags (mapconcat 'identity old " "))
	 (new (split-string (completing-read "Tags: " options nil nil current-tags) " "))
	 (added-tags (mapconcat
		      (function (lambda (x) (concat "+" x)))
		      (set-difference new old :test #'string-equal) " "))
	 (removed-tags (mapconcat
			(function (lambda (x) (concat "-" x)))
			(set-difference old new :test #'string-equal) " ")))
    (taskwarrior--mutable-shell-command "modify" id (concat added-tags " " removed-tags))))

(defun taskwarrior-edit-project ()
  "Change the project of a task."
  (interactive)
  (let* ((id      (taskwarrior-id-at-point))
	 (task    (taskwarrior-export-task id))
	 (options (split-string (shell-command-to-string "task _projects") "\n"))
	 (old     (alist-get 'project task))
	 (new     (completing-read "Project: " options nil nil old)))
    (taskwarrior--mutable-shell-command "modify" id (concat "project:" new))))

(defun taskwarrior-edit-description ()
  "Edit the description of a task."
  (interactive)
  (taskwarrior--change-attribute "description"))

(defun taskwarrior-edit-priority ()
  "Change the priority of a task."
  (interactive)
  (let* ((id (taskwarrior-id-at-point))
	 (options '("" "H" "M" "L"))
	 (new      (completing-read "Priority: " options)))
    (taskwarrior--mutable-shell-command "modify" id (concat "priority:" new))))



(defun taskwarrior-add (description)
  "Add new task with DESCRIPTION."
  (interactive "sDescription: ")
  (progn
    (taskwarrior--add description)
    (taskwarrior--revert-buffer)))

(defun taskwarrior--add (description)
  "Add new task with DESCRIPTION."
  (let ((output   (taskwarrior--shell-command "add" "" description)))
    (when (string-match "Created task \\([[:digit:]]+\\)." output)
      (match-string 1 output))))

(defun taskwarrior-mark-p ()
  "Whether there are any marked tasks."
  (and
   (boundp 'taskwarrior-marks)
   (> (length taskwarrior-marks) 0)))

(defun taskwarrior-delete ()
  "Delete task at point."
  (interactive)
  (taskwarrior-multi-action 'taskwarrior--delete "Delete?"))

(defun taskwarrior--delete (id)
  "Delete task with ID."
  (taskwarrior--mutable-shell-command "delete" id "" "" "yes"))

(defun taskwarrior-done ()
  "Mark task at point as done."
  (interactive)
  (taskwarrior-multi-action 'taskwarrior--done "Done?"))

(defun taskwarrior-multi-action (action confirmation-text)
  "Run a ACTION after processing the CONFIRMATION-TEXT."
  (when (yes-or-no-p confirmation-text)
    (if (taskwarrior-mark-p)
	(dolist (id taskwarrior-marks)
	  (funcall action id))
      (let ((id (taskwarrior-id-at-point)))
	    (funcall action id)))))

(defun taskwarrior--done (id)
  "Mark task with ID as done."
  (taskwarrior--mutable-shell-command "done" id))

(defun taskwarrior-annotate (annotation)
  "Add ANNOTATION to task at point."
  (interactive "sAnnotation: ")
  (let ((id (taskwarrior-id-at-point)))
    (taskwarrior--mutable-shell-command "annotate" id annotation)))

(defun taskwarrior--revert-buffer ()
  "Revert taskwarrior buffer."
  (let ((line-number (line-number-at-pos)))
    (taskwarrior-update-buffer)
    (goto-line line-number)))

(defun taskwarrior--mutable-shell-command (command &optional filter modifications misc confirm)
  "Run shell COMMAND with FILTER MODIFICATIONS MISC and CONFIRM."
  (let ((line-number (line-number-at-pos)))
	(taskwarrior--shell-command command filter modifications misc confirm)
	(taskwarrior-update-buffer)
	(goto-line line-number)))

(defun taskwarrior--urgency-predicate (A B)
  "Compare urgency of task A to task B."
  (let ((a (aref (cadr A) 1))
	(b (aref (cadr B) 1)))
    (>
     (string-to-number a)
     (string-to-number b))))

;; Setup a major mode for taskwarrior
;;;###autoload
(define-derived-mode taskwarrior-mode tabulated-list-mode "taskwarrior"
  "Major mode for interacting with taskwarrior. \\{taskwarrior-mode-map}"
  (setq font-lock-defaults '(taskwarrior-highlight-regexps))
  (setq tabulated-list-format
        `[("Id" 3 nil)
          ("Urg" 6 taskwarrior--urgency-predicate)
          ("Pri" 3 nil)
          ("Ann" 3 nil)
          ("Project"  15 nil)
          ("Tags"  15 nil)
          ("Description"  100 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Urg" nil)) (tabulated-list-init-header)
  (taskwarrior-update-buffer))

;;; Externally visible functions
;;;###autoload
(defun taskwarrior ()
  "Open the taskwarrior buffer.  If one already exists, bring it to the front and focus it.  Otherwise, create one and load the data."
  (interactive)
  (let* ((buf (get-buffer-create taskwarrior-buffer-name)))
      (progn
	(switch-to-buffer buf)
	;; (taskwarrior-update-buffer)
	(setq font-lock-defaults '(taskwarrior-highlight-regexps))
	(taskwarrior-mode)
	(hl-line-mode))))

(defun taskwarrior-set-due ()
  "Set due date on task."
  (interactive)
  (taskwarrior--change-attribute "due"))

(defun taskwarrior-set-scheduled ()
  "Set schedule date on task at point."
  (interactive)
  (taskwarrior--change-attribute "scheduled"))

(defun taskwarrior-set-wait ()
  "Set wait date on task at point."
  (interactive)
  (taskwarrior--change-attribute "wait"))

(defun taskwarrior-set-untl ()
  "Set until date on task at point."
  (interactive)
  (taskwarrior--change-attribute "until"))

(define-transient-command taskwarrior-date ()
  "Edit date on task"
  [["Date"
    ("d" "due"       taskwarrior-set-due)
    ("s" "scheduled" taskwarrior-set-scheduled)
    ("w" "wait"      taskwarrior-set-wait)
    ("u" "until"     taskwarrior-set-untl)]])

(defun taskwarrior--parse-date (timestamp &optional format)
  "Parse a taskwarrior TIMESTAMP into a readable string with FORMAT."
  (let* ((format (or format :date))
	 (datetime (timezone-parse-date timestamp))
	 (year (elt datetime 0))
	 (month (elt datetime 1))
	 (day (elt datetime 2))
	 (time (timezone-parse-time (elt datetime 3)))
	 (hour (elt time 0))
	 (minute (elt time 1))
	 (second (elt time 2)))
    (cond
     ((eq format :date)
      (format "%s-%s-%s" day month year hour minute second))
     ((eq format :datetime)
      (format "%s-%s-%s %s:%s:%s" day month year hour minute second)))))


(provide 'taskwarrior)
;;; taskwarrior.el ends here

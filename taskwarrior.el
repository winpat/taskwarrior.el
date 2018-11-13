;; Frontend for taskwarrior
;;
;; TODO: Implement modeline indicator for deadline and entries
;; TODO: Update buffer if command modifies the state
;; TODO: Extract "1-1000" id filter into variable
;; TODO: Figure out the difference between assoc and assoc-string
;;       (and why alist-get does not work with strings)

(require 'json)

(defgroup taskwarrior nil "An emacs frontend to taskwarrior.")

(defconst taskwarrior-mutating-commands '("add" "modify"))

(defvar taskwarrior-description 'taskwarrior-description
  "Taskwarrior mode face used for tasks with a priority of C.")

(setq taskwarrior-highlight-regexps
      `(("^[0-9]*" . font-lock-variable-name-face)
	("([0-9.]*?)" . font-lock-builtin-face)
	("\\[.*\\]" . font-lock-preprocessor-face)
	("[:space:].*:" . font-lock-function-name-face)))

(defvar taskwarrior-mode-map nil "Keymap for `taskwarrior-mode'")
(progn
  (setq taskwarrior-mode-map (make-sparse-keymap))
  (define-key taskwarrior-mode-map (kbd "p") 'taskwarrior-previous-task)
  (define-key taskwarrior-mode-map (kbd "k") 'taskwarrior-previous-task)
  (define-key taskwarrior-mode-map (kbd "n") 'taskwarrior-next-task)
  (define-key taskwarrior-mode-map (kbd "j") 'taskwarrior-next-task)
  (define-key taskwarrior-mode-map (kbd "q") 'quit-window)
  (define-key taskwarrior-mode-map (kbd "e") 'taskwarrior-change-description)
  (define-key taskwarrior-mode-map (kbd "u") 'taskwarrior-update-buffer)
  (define-key taskwarrior-mode-map (kbd "a") 'taskwarrior-add)
  (define-key taskwarrior-mode-map (kbd "d") 'taskwarrior-done)
  (define-key taskwarrior-mode-map (kbd "D") 'taskwarrior-delete)
  (define-key taskwarrior-mode-map (kbd "P") 'taskwarrior-change-project))

(defun taskwarrior--display-task-details-in-echo-area ()
  (let* ((id (taskwarrior-id-at-point))
	 (task (taskwarrior-export-task id))
	 (due (taskwarrior--parse-timestamp (alist-get 'due task))))
    (message "Due: %s" due)))

(defun taskwarrior-previous-task ()
  (interactive)
  (previous-line)
  (taskwarrior--display-task-details-in-echo-area))

(defun taskwarrior-next-task ()
  (interactive)
  (next-line)
  (taskwarrior--display-task-details-in-echo-area))

(defun taskwarrior-id-at-point ()
  (let ((line (thing-at-point 'line t)))
    (string-match "^[0-9]*" line)
    (match-string 0 line)))

(defun taskwarrior--shell-command (command &optional filter modifications miscellaneous)
  (shell-command-to-string
   (format "task %s %s %s %s"
	   (or filter "")
	   command
	   (or modifications "")
	   (or miscellaneous ""))))

(defun taskwarrior-export (filter)
  "Export taskwarrior entries as JSON"
  (vector-to-list
   (json-read-from-string
    (taskwarrior--shell-command "export" filter))))

(defun taskwarrior-load-tasks (filter)
  "Load tasks into buffer-local variable"
  (setq-local taskwarrior-tasks (taskwarrior-export filter)))

(defun taskwarrior-export-task (id)
  (let ((task (vector-to-list
	       (json-read-from-string
		(taskwarrior--shell-command "export" (concat "id:" id))))))
    (if (< (length task) 1)
	(error "Seems like two task have the same id.")
      (car task))))

(defun taskwarrior--change-attribute (attribute)
  "Change an attribute of a task"
  (let* ((prefix (concat attribute ": "))
	 (id (taskwarrior-id-at-point))
	 (task  (taskwarrior-export-task id))
	 (old-value (cdr (assoc-string attribute task)))
	 (new-value (read-from-minibuffer prefix old-value)))
    (taskwarrior--shell-command "modify" id new-value)
    (taskwarrior-update-buffer)))

(defun taskwarrior-change-description ()
  "Change the description of a task"
  (interactive)
  (taskwarrior--change-attribute "description"))

(defun taskwarrior-change-project ()
  "Change the project of a task"
  (interactive)
  (taskwarrior--change-attribute "project"))

(defun taskwarrior-add (description)
  (interactive "sDescription: ")
  (message (taskwarrior--shell-command "add" "" description))
  (taskwarrior-update-buffer))

(defun taskwarrior-done ()
  "Mark current task as done."
  (interactive)
  (let ((id (taskwarrior-id-at-point))
	(confirmation (read-from-minibuffer "Done [y/n]?: ")))
    (when (string= confirmation "y")
      (message (taskwarrior--shell-command "done" id))
      (taskwarrior-update-buffer))))

(defun taskwarrior-delete ()
  "Delete current task."
  (interactive)
  (let ((id (taskwarrior-id-at-point))
	(confirmation (read-from-minibuffer "Delete [y/n]?: ")))
    (when (string= confirmation "y")
      (taskwarrior--shell-command "config" "" "confirmation off")
      (message (taskwarrior--shell-command "delete" id))
      (taskwarrior--shell-command "config" "" "confirmation on")
      (taskwarrior-update-buffer))))

;; Setup a major mode for taskwarrior
;;;###autoload
(define-derived-mode taskwarrior-mode text-mode "taskwarrior"
  "Major mode for interacting with taskwarrior. \\{taskwarrior-mode-map}"
  (setq font-lock-defaults '(taskwarrior-highlight-regexps))
  (setq goal-column 0)
  (auto-revert-mode)
  (setq buffer-read-only t))

;;; Externally visible functions
;;;###autoload
(defun taskwarrior ()
  "Open the taskwarrior buffer.  If one already exists, bring it to
the front and focus it.  Otherwise, create one and load the data."
  (interactive)
  (taskwarrior-update-buffer))

(defun taskwarrior-update-buffer ()
  (interactive)
  (let* ((buf (get-buffer-create "taskwarrior")))
    (progn
      (switch-to-buffer buf)
      (taskwarrior-mode)
      (setq font-lock-defaults '(taskwarrior-highlight-regexps))
      (goto-char (point-min))
      (toggle-read-only)
      (erase-buffer)
      (taskwarrior-load-tasks "1-1000")
      (taskwarrior-write-entries)
      (toggle-read-only)
      (hl-line-mode)
      (goto-char (point-min))
      (while (not (equal (overlays-at (point)) nil))
	(forward-char)))))

(defun taskwarrior--sort-by-urgency (entries &optional asc)
  ;; TODO: Figure out how to store a function in the cmp variable.
  (let ((cmp (if asc '< '>)))
    (sort entries #'(lambda (x y)
		    (> (alist-get 'urgency x)
		       (alist-get 'urgency y))))))

(defun vector-to-list (vector)
  "Convert a vector to a list"
  (append vector nil))

(defun taskwarrior-write-entries ()
  (let ((entries (taskwarrior--sort-by-urgency taskwarrior-tasks)))
    (dolist (entry entries)
      (let ((id (alist-get 'id entry))
	    (urgency (alist-get 'urgency entry))
	    (project (alist-get 'project entry))
	    (description (alist-get 'description entry)))
	(insert (if project
		    (format "%-2d (%05.2f) [%s] %s\n" id urgency project description)
		    (format "%-2d (%05.2f) %s\n" id urgency description)))))))


(defun taskwarrior--parse-timestamp (ts)
  "Turn the taskwarrior timestamp into a readable format"
  (let ((year (substring ts 0 4))
	(month (substring ts 4 6))
	(day (substring ts 6 8))
	(hour (substring ts 9 11))
	(minute (substring ts 11 13))
  	(second (substring ts 13 15)))
    (format "%s-%s-%s %s:%s:%s" year month day hour minute second)))

;; Frontend for taskwarrior
;;
;; TODO: Implement modeline indicator for deadline and entries
;; TODO: Update buffer if command modifies the state
;; TODO: Extract "1-1000" id filter into variable

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
  (define-key taskwarrior-mode-map (kbd "p") 'previous-line)
  (define-key taskwarrior-mode-map (kbd "k") 'previous-line)
  (define-key taskwarrior-mode-map (kbd "n") 'next-line)
  (define-key taskwarrior-mode-map (kbd "j") 'next-line)
  (define-key taskwarrior-mode-map (kbd "q") 'quit-window)
  (define-key taskwarrior-mode-map (kbd "e") 'taskwarrior-change-description)
  (define-key taskwarrior-mode-map (kbd "u") 'taskwarrior-update-buffer)
  (define-key taskwarrior-mode-map (kbd "a") 'taskwarrior-add)
  (define-key taskwarrior-mode-map (kbd "p") 'taskwarrior-change-project))

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

(defun taskwarrior-change-description ()
  (interactive)
  (save-excursion
    (let* ((id (taskwarrior-id-at-point))
	   (task  (taskwarrior-export-task id))
	   (new-text (read-from-minibuffer "Description: " (alist-get 'description task))))
      (taskwarrior--shell-command "modify" id new-text)
      (taskwarrior-update-buffer))))

(defun taskwarrior-change-project (project)
  (interactive "sProject: ")
  (let ((filter (taskwarrior-id-at-point))
	(modifications (concat "project:" project)))
    (taskwarrior--shell-command "modify" filter modifications)
    (taskwarrior-update-buffer)))

(defun taskwarrior-add (description)
  (interactive "sDescription: ")
  (taskwarrior--shell-command "add" "" description))

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

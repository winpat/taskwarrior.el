;; Frontend for taskwarrior
;;
;; TODO: Implement modeline indicator for deadline and entries
;; TODO: Update buffer if command modifies the state

(require 'json)

(defgroup taskwarrior nil "An emacs frontend to taskwarrior.")

(defvar taskwarrior-description 'taskwarrior-description
  "Taskwarrior mode face used for tasks with a priority of C.")

(setq taskwarrior-highlight-regexps
      `(("^[0-9]*" . font-lock-variable-name-face)
	("(.*)" . font-lock-builtin-face)
	("[:space:].*:" . font-lock-function-name-face)))


(defvar taskwarrior-mode-map nil "Keymap for `taskwarrior-mode'")
(progn
  (setq taskwarrior-mode-map (make-sparse-keymap))
  (define-key taskwarrior-mode-map (kbd "p") 'previous-line)
  (define-key taskwarrior-mode-map (kbd "k") 'previous-line)
  (define-key taskwarrior-mode-map (kbd "n") 'next-line)
  (define-key taskwarrior-mode-map (kbd "j") 'next-line)
  (define-key taskwarrior-mode-map (kbd "q") 'quit-window)
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
	   (or miscellaneous))))

(defun taskwarrior-export (filter)
  "Export taskwarrior entries as JSON"
  (let ((filter (concat "id " filter))
	(command "export"))
    (json-read-from-string
     (taskwarrior--shell-command command filter))))

(defun taskwarrior-change-project (project)
  (interactive "sProject: ")
  (let ((filter (taskwarrior-id-at-point))
	(command "modify")
	(modifications (concat "project:" project)))
    (taskwarrior--shell-comand command filter modifications)))

(defun taskwarrior-add (description)
  (interactive "sDescription: ")
    (taskwarrior--shell-comand "add" "" description))

;; Setup a major mode for taskwarrior
;;;###autoload
(define-derived-mode taskwarrior-mode text-mode "taskwarrior"
  "Major mode for working with taskwarrior. \\{taskwarrior-mode-map}"
  (setq font-lock-defaults '(taskwarrior-highlight-regexps))
  (setq goal-column 0)
  (auto-revert-mode)
  (setq buffer-read-only t))

;;; externally visible functions
;;;###autoload
(defun taskwarrior ()
  "Open the taskwarrior buffer.  If one already exists, bring it to
the front and focus it.  Otherwise, create one and load the data."
  (interactive)
  (let* ((buf (get-buffer-create "taskwarrior")))
    (progn
      (switch-to-buffer buf)
      (setq font-lock-defaults '(taskwarrior-highlight-regexps))
      (toggle-read-only)
      (goto-char (point-min))
      (erase-buffer)
      (taskwarrior-write-entries)
      (taskwarrior-mode)
      (while (not (equal (overlays-at (point)) nil))
	(forward-char)))))

(defun taskwarrior-write-entries ()
  (let ((entries (append (taskwarrior-export "context:personal") nil)))
    (dolist (entry entries)
      (progn
	(insert (format
		 "%-2d (%0.2f) %s: %s?\n"
		 (alist-get 'id entry)
		 (alist-get 'urgency entry)
		 (alist-get 'project entry)
		 (alist-get 'description entry)))))))

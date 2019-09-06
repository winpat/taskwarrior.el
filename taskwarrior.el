;; Frontend for taskwarrior
;;
;; TODO: Implement modeline indicator for deadline and entries
;; TODO: Update buffer if command modifies the state
;; TODO: Extract "1-1000" id filter into variable
;; TODO: Figure out the difference between assoc and assoc-string
;;       (and why alist-get does not work with strings)
;; TODO: Restore position after taskwarrior-update-buffer is called

(require 'json)

(defgroup taskwarrior nil "An emacs frontend to taskwarrior.")

(defvar taskwarrior-buffer-name "*taskwarrior*")

(defvar taskwarrior-description 'taskwarrior-description
  "Taskwarrior mode face used for tasks with a priority of C.")

(defvar taskwarrior-profile-alist 'taskwarrior-profile-alist
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

(defvar taskwarrior-mode-map nil "Keymap for `taskwarrior-mode'")
(progn
  (setq taskwarrior-mode-map (make-sparse-keymap))
  (define-key taskwarrior-mode-map (kbd "q") 'quit-window)
  (define-key taskwarrior-mode-map (kbd "e") 'taskwarrior-change-description)
  (define-key taskwarrior-mode-map (kbd "U") 'taskwarrior-edit-priority)
  (define-key taskwarrior-mode-map (kbd "g") 'taskwarrior-update-buffer)
  (define-key taskwarrior-mode-map (kbd "a") 'taskwarrior-add)
  (define-key taskwarrior-mode-map (kbd "A") 'taskwarrior-annotate)
  (define-key taskwarrior-mode-map (kbd "d") 'taskwarrior-done)
  (define-key taskwarrior-mode-map (kbd "l") 'taskwarrior-load-profile)
  (define-key taskwarrior-mode-map (kbd "o") 'taskwarrior-open-annotation)
  (define-key taskwarrior-mode-map (kbd "D") 'taskwarrior-delete)
  (define-key taskwarrior-mode-map (kbd "m") 'taskwarrior-mark-task)
  (define-key taskwarrior-mode-map (kbd "u") 'taskwarrior-unmark-task)
  (define-key taskwarrior-mode-map (kbd "f") 'taskwarrior-filter)
  (define-key taskwarrior-mode-map (kbd "r") 'taskwarrior-reset-filter)
  (define-key taskwarrior-mode-map (kbd "t") 'taskwarrior-edit-tags)
  (define-key taskwarrior-mode-map (kbd "RET") 'taskwarrior-info)
  (define-key taskwarrior-mode-map (kbd "P") 'taskwarrior-edit-project))

(defun taskwarrior-load-profile ()
  (interactive)
  (let* ((profiles (-map 'car taskwarrior-profile-alist))
	 (profile (completing-read "Profile: " profiles))
	 (filter (cdr (assoc-string profile taskwarrior-profile-alist))))
    (progn
      (taskwarrior--set-filter filter)
      (taskwarrior-update-buffer filter))))

(defun taskwarrior-unmark-task ()
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
  (interactive)
  (let* ((id (taskwarrior-id-at-point))
	 (task  (taskwarrior-export-task id))
	 (annotations (-map (lambda (x) (alist-get 'description x)) (vector-to-list (alist-get 'annotations task))))
	 (choice (completing-read "Tags: " annotations)))
    (org-open-link-from-string choice)))

(defun taskwarrior-info ()
  (interactive)
  (let* ((id (taskwarrior-id-at-point))
	 (buf (get-buffer-create "*taskwarrior info*")))
    (progn
      (switch-to-buffer-other-window buf)
      (insert (taskwarrior--shell-command "info" "" id)))))

(defun taskwarrior--parse-created-task-id (output)
  (when (string-match "^.*Created task \\([0-9]+\\)\\.*$" output)
    (message (match-string 1 output))))

(defun taskwarrior--parse-org-link (link)
    (string-match org-bracket-link-regexp link)
    (list
     (match-string 1 link)
     (match-string 3 link)))

(defun taskwarrior-capture (arg)
  (interactive "P")
  (let* ((link (car (cdr (taskwarrior--parse-org-link (org-store-link arg)))))
	 (description (read-from-minibuffer "Description: "))
	 (id (taskwarrior--parse-created-task-id
	      (shell-command-to-string (format "task add %s" description)))))
    (shell-command-to-string (format "task %s annotate %s" id link))))

(defun taskwarrior-id-at-point ()
  (let ((line (thing-at-point 'line t)))
    (string-match "^  [0-9]*" line)
    (string-trim-left (match-string 0 line))))

(defun taskwarrior--get-filter-as-string ()
  (if (local-variable-p 'taskwarrior-active-filters)
      (mapconcat 'identity taskwarrior-active-filters " ")
    ""))

(defun taskwarrior--set-filter (filter)
  (cond ((stringp filter) (setq-local taskwarrior-active-filters (split-string filter " ")))
	((listp filter) (setq-local taskwarrior-active-filters filter))
	(t (error "Filter did not match any supported type."))))

(defun taskwarrior-reset-filter ()
  (interactive)
  (progn
    (taskwarrior--set-filter "")
    (taskwarrior-update-buffer "")))

(defun taskwarrior-filter ()
  (interactive)
  (let ((new-filter (read-from-minibuffer "Filter: " (taskwarrior--get-filter-as-string))))
    (progn
      (taskwarrior--set-filter new-filter)
      (taskwarrior-update-buffer new-filter))))

(defun taskwarrior--shell-command (command &optional filter modifications miscellaneous confirm)
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

(defun vector-to-list (vector)
  "Convert a vector to a list"
  (append vector nil))

(defun taskwarrior--concat-tag-list (tags)
  (mapconcat
   (function (lambda (x) (format "+%s" x)))
   (vector-to-list tags)
   " "))

(defun taskwarrior-export ()
  "Turn task export into the tabulated list entry form"
  (mapcar
   (lambda (entry)
     (let* ((id          (format "%s" (alist-get 'id entry)))
	    (urgency     (format "%0.2f" (alist-get 'urgency entry)))
	    (priority    (format " %s " (or (alist-get 'priority entry) "")))
	    (annotations (format "%d"  (length (or (alist-get 'annotations entry) '()))))
	    (project     (or (format "%s" (alist-get 'project entry)) ""))
	    (tags        (or (taskwarrior--concat-tag-list (alist-get 'tags entry)) ""))
	    (description (format "%s" (alist-get 'description entry))))
       `(,id [,id ,urgency ,priority ,annotations ,project ,tags ,description])))
   (vector-to-list
    (json-read-from-string
     (taskwarrior--shell-command "export" "id.not:0")))))

(defun taskwarrior-update-buffer (&optional filter)
  (interactive)
  (let* ((filter (taskwarrior--get-filter-as-string)))
    (progn
      (setq tabulated-list-entries (taskwarrior-export))
      (tabulated-list-print t)
      (goto-char (point-min))
      (while (not (equal (overlays-at (point)) nil))
	(forward-char))
      (taskwarrior--set-filter filter))))

(defun taskwarrior-export-task (id)
  (let ((task (vector-to-list
	       (json-read-from-string
		(taskwarrior--shell-command "export" (concat "id:" id))))))
    (if (< (length task) 1)
	(error "Seems like two task have the same id.")
      (car task))))

(defun taskwarrior--change-attribute (attribute)
  "Change an attribute of a task"
  (let* ((prefix (concat attribute ":"))
	 (id (taskwarrior-id-at-point))
	 (task  (taskwarrior-export-task id))
	 (old-value (cdr (assoc-string attribute task)))
	 (new-value (read-from-minibuffer (concat prefix " ") old-value))
         (quoted-value (concat "\"" new-value "\"")))
    (taskwarrior--mutable-shell-command "modify" id (concat prefix quoted-value))))

(defun taskwarrior-edit-tags ()
  (interactive)
  (let* ((id (taskwarrior-id-at-point))
	 (task (taskwarrior-export-task id))
	 (options (split-string (shell-command-to-string "task _tags") "\n"))
	 (old (vector-to-list (alist-get 'tags task)))
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
  "Change the project of a task"
  (interactive)
  (let* ((id      (taskwarrior-id-at-point))
	 (task    (taskwarrior-export-task id))
	 (options (split-string (shell-command-to-string "task _projects") "\n"))
	 (old     (alist-get 'project task))
	 (new     (completing-read "Project: " options nil nil old)))
    (taskwarrior--mutable-shell-command "modify" id (concat "project:" new))))

(defun taskwarrior-change-description ()
  "Change the description of a task"
  (interactive)
  (taskwarrior--change-attribute "description"))

(defun taskwarrior-edit-priority ()
  "Change the priority of a task"
  (interactive)
  (let* ((id (taskwarrior-id-at-point))
	 (options '("" "H" "M" "L"))
	 (new      (completing-read "Priority: " options)))
    (taskwarrior--mutable-shell-command "modify" id (concat "priority:" new))))

(defun taskwarrior-add (description)
  (interactive "sDescription: ")
  (taskwarrior--mutable-shell-command "add" "" description))

(defun taskwarrior-mark-p ()
  "Whether there are any marked tasks"
  (and
   (boundp 'taskwarrior-marks)
   (> (length taskwarrior-marks) 0)))

(defun taskwarrior-delete ()
  (interactive)
  (taskwarrior-multi-action 'taskwarrior--delete "Delete?"))

(defun taskwarrior--delete (id)
  "Delete task with id."
  (taskwarrior--mutable-shell-command "delete" id "" "" "yes"))

(defun taskwarrior-done ()
  (interactive)
  (taskwarrior-multi-action 'taskwarrior--done "Done?"))

(defun taskwarrior-multi-action (action confirmation-text)
  (when (yes-or-no-p confirmation-text)
    (if (taskwarrior-mark-p)
	(dolist (id taskwarrior-marks)
	  (funcall action id))
      (let ((id (taskwarrior-id-at-point)))
	    (funcall action id)))))

(defun taskwarrior--done (id)
  "Mark task as done."
  (taskwarrior--mutable-shell-command "done" id))

(defun taskwarrior-annotate (annotation)
  "Delete current task."
  (interactive "sAnnotation: ")
  (let ((id (taskwarrior-id-at-point)))
    (taskwarrior--mutable-shell-command "annotate" id annotation)))

(defun taskwarrior--mutable-shell-command (command &optional filter modifications misc confirm)
  "Run shell command and restore taskwarrior buffer."
  (let ((line-number (line-number-at-pos)))
	(taskwarrior--shell-command command filter modifications misc confirm)
	(taskwarrior-update-buffer)
	(goto-line line-number)))

(defun taskwarrior--urgency-predicate (A B)
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
  (taskwarrior-update-buffer ""))

;;; Externally visible functions
;;;###autoload
(defun taskwarrior ()
  "Open the taskwarrior buffer.  If one already exists, bring it to
the front and focus it.  Otherwise, create one and load the data."
  (interactive)
  (let* ((buf (get-buffer-create taskwarrior-buffer-name)))
      (progn
	(switch-to-buffer buf)
	;; (taskwarrior-update-buffer)
	(setq font-lock-defaults '(taskwarrior-highlight-regexps))
	(taskwarrior-mode)
	(hl-line-mode))))

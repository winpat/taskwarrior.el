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

(defconst taskwarrior-mutating-commands '("add" "modify"))

(defvar taskwarrior-description 'taskwarrior-description
  "Taskwarrior mode face used for tasks with a priority of C.")

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
  (define-key taskwarrior-mode-map (kbd "p") 'taskwarrior-previous-task)
  (define-key taskwarrior-mode-map (kbd "k") 'taskwarrior-previous-task)
  (define-key taskwarrior-mode-map (kbd "n") 'taskwarrior-next-task)
  (define-key taskwarrior-mode-map (kbd "j") 'taskwarrior-next-task)
  (define-key taskwarrior-mode-map (kbd "q") 'quit-window)
  (define-key taskwarrior-mode-map (kbd "e") 'taskwarrior-change-description)
  (define-key taskwarrior-mode-map (kbd "U") 'taskwarrior-change-priority)
  (define-key taskwarrior-mode-map (kbd "g") 'taskwarrior-update-buffer)
  (define-key taskwarrior-mode-map (kbd "a") 'taskwarrior-add)
  (define-key taskwarrior-mode-map (kbd "d") 'taskwarrior-done)
  (define-key taskwarrior-mode-map (kbd "o") 'taskwarrior-open-annotation)
  (define-key taskwarrior-mode-map (kbd "D") 'taskwarrior-delete)
  (define-key taskwarrior-mode-map (kbd "m") 'taskwarrior-mark-task)
  (define-key taskwarrior-mode-map (kbd "u") 'taskwarrior-unmark-task)
  (define-key taskwarrior-mode-map (kbd "f") 'taskwarrior-filter)
  (define-key taskwarrior-mode-map (kbd "r") 'taskwarrior-reset-filter)
  (define-key taskwarrior-mode-map (kbd "t") 'taskwarrior-edit-tags)
  (define-key taskwarrior-mode-map (kbd "RET") 'taskwarrior-info)
  (define-key taskwarrior-mode-map (kbd "P") 'taskwarrior-change-project))


(defun test ()
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (string-match (rx "* 1"))
    (string-trim-left (match-string 0 line))))

(defun taskwarrior--display-task-details-in-echo-area ()
  (let* ((id (taskwarrior-id-at-point))
	 (task (taskwarrior-export-task id))
	 (annotation-count (length (alist-get 'annotations task)))
	 (due (taskwarrior--parse-timestamp (alist-get 'due task))))
    (when due
      (message "Due: %s | %i Annotations" due annotation-count))))

(defun taskwarrior-previous-task ()
  (interactive)
  (previous-line)
  (taskwarrior--display-task-details-in-echo-area))

(defun taskwarrior-next-task ()
  (interactive)
  (next-line)
  (taskwarrior--display-task-details-in-echo-area))

(defun taskwarrior-unmark-task ()
  (interactive)
  (let ((id (taskwarrior-id-at-point)))
    (if (local-variable-p 'taskwarrior-marks)
	(setq-local taskwarrior-marks (remove id taskwarrior-marks)))))

(defun taskwarrior-mark-task ()
  (interactive)
  (let ((id (taskwarrior-id-at-point)))
    (progn
      (if (local-variable-p 'taskwarrior-marks)
	  (setq-local taskwarrior-marks (delete-dups (cons id taskwarrior-marks)))
	(setq-local taskwarrior-marks (list id))))
    (save-excursion
      (read-only-mode -1)
      (beginning-of-line)
      (insert "*")
      (read-only-mode 1))))

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

(defun taskwarrior-org-store-link (arg)
  (interactive "P")
  (let* ((link (org-store-link arg))
	 (description (read-from-minibuffer "Description: "))
	 (id (taskwarrior--parse-created-task-id
	      (shell-command-to-string (format "task add %s" description)))))
    (shell-command-to-string (format "task %s annotate %s" id link))))

(defun taskwarrior-id-at-point ()
  (let ((line (thing-at-point 'line t)))
    (string-match "^ [0-9]*" line)
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

(defun taskwarrior-change-description ()
  "Change the description of a task"
  (interactive)
  (taskwarrior--change-attribute "description"))

(defun taskwarrior-change-priority ()
  "Change the priority of a task"
  (interactive)
  (taskwarrior--change-attribute "priority"))

(defun taskwarrior-change-project ()
  "Change the project of a task"
  (interactive)
  (taskwarrior--change-attribute "project"))

(defun taskwarrior-add (description)
  (interactive "sDescription: ")
  (taskwarrior--mutable-shell-command "add" "" description))

(defun taskwarrior-done ()
  "Mark current task as done."
  (interactive)
  (let ((id (taskwarrior-id-at-point))
	(confirmation (yes-or-no-p "Done?")))
    (when confirmation
	(taskwarrior--mutable-shell-command "done" id))))

(defun taskwarrior-delete ()
  "Delete current task."
  (interactive)
  (let ((id (taskwarrior-id-at-point))
	(confirmation (yes-or-no-p "Delete?")))
    (when confirmation
	  (taskwarrior--mutable-shell-command "delete" id "" "" "yes"))))

(defun taskwarrior--mutable-shell-command (command &optional filter modifications misc confirm)
  "Run shell command and restore taskwarrior buffer."
  (let ((line-number (line-number-at-pos)))
	(taskwarrior--shell-command command filter modifications misc confirm)
	(taskwarrior-update-buffer)
	(goto-line line-number)))


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
  (let* ((buf (get-buffer-create taskwarrior-buffer-name)))
      (progn
	(switch-to-buffer buf)
	(taskwarrior-update-buffer)
	(setq font-lock-defaults '(taskwarrior-highlight-regexps))
	(taskwarrior-mode)
	(hl-line-mode))))

(defun taskwarrior-update-buffer (&optional filter)
  (interactive)
  (let ((filter (taskwarrior--get-filter-as-string)))
    (progn
      (read-only-mode -1)
      (erase-buffer)
      (taskwarrior-load-tasks (concat "1-1000 " filter))
      (taskwarrior-write-entries)
      (read-only-mode t)
      (goto-char (point-min))
      (while (not (equal (overlays-at (point)) nil))
	(forward-char))
      (taskwarrior--set-filter filter))))

(defun taskwarrior--sort-by-urgency (entries &optional asc)
  ;; TODO: Figure out how to store a function in the cmp variable.
  (let ((cmp (if asc '< '>)))
    (sort entries #'(lambda (x y)
		    (> (alist-get 'urgency x)
		       (alist-get 'urgency y))))))

(defun vector-to-list (vector)
  "Convert a vector to a list"
  (append vector nil))


(defun taskwarrior--get-max-length (key lst)
  "Get the length of the longst element in a list"
  (apply 'max
	 (-map 'length
	       (-map (-partial 'alist-get key) lst))))

(defun taskwarrior-write-entries ()
  (let ((entries (taskwarrior--sort-by-urgency taskwarrior-tasks)))
    (dolist (entry entries)
      (let* ((id                 (format "%-2d" (alist-get 'id entry)))
	     (urgency            (format "(%05.2f)" (alist-get 'urgency entry)))
	     (priority           (format "%s" (or (alist-get 'priority entry) " ")))
	     (project            (format "[%s]" (alist-get 'project entry)))
	     (tags               (format "%s" (taskwarrior--concat-tag-list (alist-get 'tags entry))))
	     ;; (project-max-length (taskwarrior--get-max-length 'project entries))
	     ;; (project-spacing    (- project-max-length (length project)))
	     (description        (alist-get 'description entry)))
	  (insert (concat " " id " " urgency " " priority " " project " " tags " " description "\n"))))))


(defun taskwarrior--concat-tag-list (tags)
  (mapconcat (function (lambda (x) (format "+%s" x))) (vector-to-list tags) " "))

(defun taskwarrior--parse-timestamp (ts)
  "Turn the taskwarrior timestamp into a readable format"
  (if (not ts)
      ts
    (let ((year   (substring ts 0 4))
	  (month  (substring ts 4 6))
	  (day    (substring ts 6 8))
	  (hour   (substring ts 9 11))
	  (minute (substring ts 11 13))
  	  (second (substring ts 13 15)))
      (format "%s-%s-%s %s:%s:%s" year month day hour minute second))))


(global-set-key (kbd "C-x t") 'taskwarrior)

;;; taskwarrior-test.el --- taskwarrior unit tests

;;; Commentary:

;; Run through make target `m̀ake test`

;;; Code:

(require 'ert)

(ert-deftest taskwarrior-add-task-test ()
  "Ensure that special characters such as quotes and parens are properly escaped when adding new tasks"
  (let* ((task-id (taskwarrior--add "project:ert +emacs \"Write test suite for taskwarrior.el (using ERT)\""))
	 (task (taskwarrior-export-task task-id)))
    (should (string= (alist-get 'project task) "ert"))
    (should (string= (aref (alist-get 'tags task) 0) "emacs"))
    (should (string= (alist-get 'description task) "Write test suite for taskwarrior.el (using ERT)"))))

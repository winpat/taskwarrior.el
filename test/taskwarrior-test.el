;;; taskwarrior-test.el --- Taskwarrior test suite -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Patrick Winter
;;
;; License: GPLv3

;;; Commentary:

;;; Code:

(require 'taskwarrior)

(ert-deftest taskwarrior-add-task ()
  "Ensure that special characters such as quotes and parens are properly escaped when adding new tasks"
  (let* ((task-id (taskwarrior--add "project:ert +emacs \"Write test suite for taskwarrior.el (using ERT)\""))
	 (task (taskwarrior-export-task task-id)))
    (should (string= (alist-get 'project task) "ert"))
    (should (string= (aref (alist-get 'tags task) 0) "emacs"))
    (should (string= (alist-get 'description task) "Write test suite for taskwarrior.el (using ERT)"))))

(ert-deftest taskwarrior-parse-task-id ()
  (should (string= (taskwarrior--parse-created-task-id "Created task 9.") "9"))
  (should (string= (taskwarrior--parse-created-task-id "Created task 1000.") "1000"))
  (should (string= (taskwarrior--parse-created-task-id "Some invalid input") nil)))


(provide 'taskwarrior-test)
;;; taskwarrior-test.el ends here

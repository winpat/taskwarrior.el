Taskwarrior.el
==============
An emacs frontend for taskwarrior.


Annotations
-----------
Taskwarrior already has the option to add annotations to a task.

(taskwarrior-create-task-from-dired-file)
(taskwarrior-create-task-from-mu4e-mail)

Capture Templates
-----------------
With taskwarrior you often spending way to many keystrokes on capturing metadata around a new task
(Especially if you are a heavy user of UDA's). Capture templates to the rescue:

```
(taskwarrior-capture
  "Work task"
  "context:work customer:? project:?)
```

The `?` automatically present autocompletion options based on `task _unique` in case of UDA or `task _...`


Review
------
Quiet often you want to review a list of different
(taskwarrior-review
	'("contex:work" "context:school")

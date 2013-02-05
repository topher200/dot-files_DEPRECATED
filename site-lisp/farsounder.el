;;; farsounder.el --- FarSounder dev tools

(defgroup farsounder nil
  "Emacs stuff for FarSounder software development."
  :group 'programming)

(defun fs-insert-header ()
  "Insert FarSounder's standard header comment in the current buffer.
Works with C++ and python files."
  (interactive)
  (goto-char (point-min))
  (cond
   ((eq major-mode 'c++-mode)
    (insert "// TODO(topher) project name
// Copyright FarSounder Inc. All rights reserved.
// For internal use only. See NOTICE.txt for details.

"))
   ((eq major-mode 'python-mode)
    (insert "# Copyright FarSounder Inc. All rights reserved.
# For internal use only. See NOTICE.txt for details.

\"\"\"TODO(topher) add description
\"\"\"

"))
   (t (message "Don't know what header to add."))))

(provide 'farsounder)

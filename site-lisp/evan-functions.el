(defun google ()
  "Search google.

Prompts for search query, with default being current thing under
the cursor. Results are shown in a w3m session or in the system's
web browser depending on user input."
  (interactive)
  (let (query)
    (setq query (read-from-minibuffer "Search for: " (thing-at-point 'symbol)))
    (if (y-or-n-p "Show results in emacs? ")
        (w3m (concat "http://www.google.com/search?q=" query))
      (browse-url (concat "http://www.google.com/search?q=" query)))))

(defun toc ()
  "Poor man's table of contents for the current python/lisp buffer."
  (interactive)
  (cond
   ((eq major-mode 'c++-mode)
    (lgrep "^[A-Za-z0-9_]* \\?[A-Za-z0-9_*&]\\+ [A-Za-z0-9_]*::[A-Za-z0-9_]* \\?("
           (buffer-name) ""))
   ((eq major-mode 'emacs-lisp-mode)
    (lgrep "^ *(\\(defcustom\\|defun\\|defgroup\\) [A-Za-z0-9_-]*" (buffer-name) ""))
   ((eq major-mode 'python-mode)
    (grep (concat "grep -n \"^ *\\(def\\|class\\) [A-Za-z0-9_]*\" "
                  (buffer-name) "")))
   (t (message "Don't know how to do TOC for this buffer!"))))

(defun ushell (shell-buffer-name)
  "Unique Shell: Create a shell with a user specified buffer name postfix."
  (interactive "*sBuffer Name: ")
  (shell (concat "shell-" shell-buffer-name)))

(defun named-shell ()
  "Named Shell: Create a shell with the name of the current buffer."
  (interactive)
  (shell (concat "shell-" (buffer-name))))

(provide 'evan-functions)

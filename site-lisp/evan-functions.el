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

(provide 'evan-functions)

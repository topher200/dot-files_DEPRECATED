;; python grin support for emacs. Based on ack support script from:
;; http://stackoverflow.com/questions/2322389/ack-does-not-work-when-run-from-grep-find-in-emacs-on-windows

(defcustom grin-command (or (executable-find "grin")
                            (executable-find "grin-grep"))
  "Command to use to call grin"
  :type 'file)

(defvar grin-command-line (concat grin-command " --emacs -D "))
(defvar grin-history nil)
(defvar grin-host-defaults-alist nil)

(defun grin-base ()
  "Like ack, but using grin as the default"
  (interactive)
  ; Make sure grep has been initialized
  (if (>= emacs-major-version 22)
      (require 'grep)
    (require 'compile))
  ; Close STDIN to keep grin from going into filter mode
  (let ((null-device (format "< %s" null-device))
        (grep-command grin-command-base)
        (grep-history grin-history)
        (grep-host-defaults-alist grin-host-defaults-alist))
    (call-interactively 'grep)
    (setq grin-history             grep-history
          grin-host-defaults-alist grep-host-defaults-alist)))


(defun grin ()
  "Like ack, but using grin as the default"
  (interactive)
  (setq grin-command-base grin-command-line)
  (call-interactively 'grin-base))

(defun grin-ss ()
  "Runs grin from Sonasoft root"
  (interactive)
  (setq grin-command-base (concat "cd d:/dev/sonasoft/src && "
                                   grin-command-line))
  (call-interactively 'grin-base))

(provide 'grin)

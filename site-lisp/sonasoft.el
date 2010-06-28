;;; sonasoft.el --- SonaSoft dev tools

;; NOTE: See
;; http://neugierig.org/software/git/?url=config/plain/emacs.d/chromium.el on
;; how to modify this to allow for automatically setting sonasoft-root as a
;; buffer local variable based on the source file's path.

(defgroup sonasoft nil
  "Emacs settings for SonaSoft development."
  :group 'programming)

(defcustom sonasoft-build-command "/build Debug"
  "Build command used by `compile'."
  :type 'string
  :group 'sonasoft)

(defcustom sonasoft-root "d:/sonasoft/src"
  "SonaSoft src root directory. Used when setting up `compile-command'."
  :type 'string
  :group 'sonasoft)

(defcustom sonasoft-visualstudio  "\"c:/Program Files/Microsoft Visual Studio 9.0/Common7/IDE/devenv.com\""
  "Visual studio binary to build with. Used by `compile'."
  :type 'string
  :group 'sonasoft)

(defun sonasoft-setup-compile ()
  "Set up `compile' to default to SonaSoft's compile command."
  (interactive)
  (set 'compile-command
       (concat sonasoft-visualstudio " \"" sonasoft-root "/Sonasoft.sln\" "
               sonasoft-build-command)))

(provide 'sonasoft)

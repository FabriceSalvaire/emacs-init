;;; bootstrap.el --- Init -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar init--module-options nil)

(defun init--load-modules (args)
  "Helper to load Lisp files."
  (catch ':stop
    (dolist (module args)
      (if (eq module ':stop)
          (progn
            (message "Stop sub-module loading")
            (throw ':stop nil))
        (let ((fist-char (substring (symbol-name module) 0 1)))
          (if (equal fist-char "+")
              (push  module init--module-options)
            (let ((path (concat (symbol-name module) ".el")))
              ;; (load FILE &optional NOERROR NOMESSAGE NOSUFFIX MUST-SUFFIX)
              ;; (load path t t)
              (load path nil nil)
              )))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(init--load-modules '(
               ;; instead to comment several lines to stop at some point
               ;; use keyword
               ;;   :stop

               ;; 0 load early-init.el
               ;; 1 load this init.el...

               ;; Fixme: move
               ;; load internal libraries
               doom-lib/doom-lib
               doom-lib/buffers
               doom-lib/plist
               doom-lib/ui
               doom-lib/fonts
               ;; loaded in -theme.el
               ;; doom-lib/doom-themes-base
               ;; doom-lib/doom-themes
               doom-lib/doom-ui

               core/startup
               ;; packages
               core/packages-straight
               doom-lib/keys
               core/keybinding
               core/ui
               core/core
               ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; bootstrap.el ends here

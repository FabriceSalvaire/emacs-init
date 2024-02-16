;;; bootstrap.el --- Init -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Hooks

(defcustom doom-before-modules-init-hook nil
  "Hooks run before module init.el files are loaded."
  :group 'doom
  :type 'hook)

(defcustom doom-after-modules-init-hook nil
  "Hooks run after module init.el files are loaded."
  :group 'doom
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "doom-lib/doom-lib.el")

(defvar init--module-options nil)

;; Fixme: load doom-libe befora and merge
(defun init--load-modules-private (args)
  "Helper to load Lisp files."
  (progn
    (catch ':stop
      (dolist (module args)
        (if (eq module ':stop)
            (progn
              (message "Stop sub-module loading")
              (throw ':stop nil))
          (let ((fist-char (substring (symbol-name module) 0 1)))
            (if (equal fist-char "+")
                (push  module init--module-options)
              (let ((path (concat (symbol-name module) ".el"))
                    (load-time (float-time)))
                ;; (load FILE &optional NOERROR NOMESSAGE NOSUFFIX MUST-SUFFIX)
                ;; (load path t t)
                (load path nil nil)
                (doom-log "loaded '%s' (%.1f us)"
                          path
                          (* (- (float-time) load-time) 1000))
                ))))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(init--load-modules-private '(
               ;; instead to comment several lines to stop at some point
               ;; use keyword
               ;;   :stop

               ;; 0 load early-init.el
               ;; 1 load this init.el...

               ;; Fixme: move
               ;; load internal libraries
               ;; doom-lib/doom-lib ; loaded before
               doom-lib/buffers
               doom-lib/plist
               doom-lib/ui
               doom-lib/fonts
               ;; loaded in -theme.el
               ;; doom-lib/doom-themes-base
               ;; doom-lib/doom-themes
               doom-lib/doom-ui

               startup
               ;; packages
               packages-straight
               doom-lib/keys
               keybinding
               ui
               core
               ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init--load-modules (args)
  "Helper to load Lisp files."
  (progn
    (doom-run-hooks 'doom-before-modules-init-hook)
    (init--load-modules-private args)
    ;; see doom-emacs/lisp/doom-profiles.el
    (doom-run-hooks 'doom-after-modules-init-hook)
    ))

;;; bootstrap.el ends here

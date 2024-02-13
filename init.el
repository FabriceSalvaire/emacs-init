;;; init.el --- Init -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; See also early-init.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Top Emacs Configuration
;;

;; M-x load-file ~./emacs
;;
;; C-x C-f /path/to/... .el RET
;; M-x byte-compile-file RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Path Settings

;; user-emacs-directory -> "~/.config/emacs-legacy/"

(defconst local_path_prefix "/home/common/emacs") ; this is the only absolut path for Emacs files
;; (defconst local_checkout_path (file-name-concat local_path_prefix "checkout"))
;; (defconst local_emacs_site_lisp_path (file-name-concat local_checkout_path "emacs-site-lisp"))
(defconst local_emacs_d_path (file-name-concat local_path_prefix "lisp"))
(defconst local_theme_path (file-name-concat local_path_prefix "themes"))

(add-to-list 'load-path local_emacs_d_path)
(add-to-list 'load-path local_theme_path)
;; (add-to-list 'load-path local_emacs_site_lisp_path)
(add-to-list 'custom-theme-load-path local_theme_path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

               startup
               ;; packages
               packages-straight
               keybinding
               ui
               core

               user-functions

               ;; completion/previous ; buffer switch
               completion/company
               completion/vertico
               edition
               ;; undo
               speller

               file-manager
               sysadmin

               tree-sitter
               ;; code-completion
               magit
               ;; lang/lang
               lang/emacs-lisp
               lang/markdown
               lang/c-java
               lang/python
               lang/web
               checker
               git
               +foo

               ;; finally
               post-keybinding
               ))

(message "init--module-options %s" init--module-options)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom Settings

(setq custom-file (file-name-concat local_path_prefix "gnu-emacs-custom.el"))
(load custom-file t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1024 1024))

;;; init.el ends here

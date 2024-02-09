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
;; (add-to-list 'load-path local_emacs_site_lisp_path)
(add-to-list 'custom-theme-load-path local_theme_path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun _load-files (args)
  "Helper to load Lisp files."
  (catch ':stop
    (dolist (_ args)
      (if (eq _ ':stop)
          (progn
            (message "Stop sub-module loading")
            (throw ':stop _))
        (let ((path (concat (symbol-name _) ".el")))
          ;; (load FILE &optional NOERROR NOMESSAGE NOSUFFIX MUST-SUFFIX)
          ;; (load path t t)
          (load path nil nil)
          )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(_load-files '(
               ;; :stop

               ;; 0 load early-init.el
               ;; 1 load this init.el...

               ;; load internal libraries
               doom-lib/doom-lib
               doom-lib/buffers
               doom-lib/plist
               doom-lib/ui

               startup
               ;; packages
               packages-straight
               keybinding
               ui
               core

               user-functions

               ;; :skip
               completion/previous ; buffer switch
               edition
               ;; undo
               speller

               file-manager
               sysadmin

               tree-sitter
               code-completion
               magit
               ;; lang/lang
               lang/markdown
               lang/c-java
               lang/python
               lang/web
               checker
               git

               ;; must be after
               post-keybinding
               ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom Settings

(setq custom-file (file-name-concat local_path_prefix "gnu-emacs-custom.el"))
(load custom-file t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1024 1024))

;;; init.el ends here

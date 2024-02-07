;;;init.el --- Init -*- lexical-binding: t; -*-

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
(defconst local_checkout_path (file-name-concat local_path_prefix "checkout"))
(defconst local_emacs_site_lisp_path (file-name-concat local_checkout_path "emacs-site-lisp"))
(defconst local_emacs_d_path (file-name-concat local_path_prefix "lisp"))
(defconst local_theme_path (file-name-concat local_path_prefix "themes"))

(add-to-list 'load-path local_emacs_d_path)
(add-to-list 'load-path local_emacs_site_lisp_path)
(add-to-list 'custom-theme-load-path local_theme_path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun _load-files (args)
  "Helper to load lisp files"
  (dolist (_ args)
    ;; (load FILE &optional NOERROR NOMESSAGE NOSUFFIX MUST-SUFFIX)
    (let ((path (concat (symbol-name _) ".el")))
      ;; (load path t t)
      (load path nil nil)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(_load-files '(
               doom-lib/doom-lib
               doom-lib/buffers
               doom-lib/ui
               startup

               packages
               ;; packages-straight
               frame
               core

               user-functions

               completion ; buffer switch
               edition
               undo
               speller

               file-manager
               sysadmin

               tree-sitter
               code-completion
               lang/lang
               lang/c-java
               lang/python
               lang/web
               checker
               git

               ;; must be after
               keys

               server
               ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom Settings

(setq custom-file (file-name-concat local_path_prefix "gnu-emacs-custom.el"))
(load custom-file t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1024 1024))

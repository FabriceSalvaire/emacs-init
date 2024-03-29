;;; init.el --- Init -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; See also `early-init.el` and `lisp/code/REAMDE.md`

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

;; user-emacs-directory -> "~/.config/emacs/"

(defconst local_path_prefix "/home/common/emacs") ; this is the only absolut path for Emacs files
(defconst local_emacs_d_path (file-name-concat local_path_prefix "lisp"))
(defconst local_theme_path (file-name-concat local_path_prefix "themes"))

(add-to-list 'load-path local_emacs_d_path)
(add-to-list 'load-path (file-name-concat local_emacs_d_path "doom-lib"))
(add-to-list 'load-path (file-name-concat local_emacs_d_path "core"))
(add-to-list 'load-path (file-name-concat local_emacs_d_path "modules"))
(add-to-list 'load-path local_theme_path)

(add-to-list 'custom-theme-load-path local_theme_path)

;; (add-to-list 'load-path local_emacs_site_lisp_path)
;; (defconst local_checkout_path (file-name-concat local_path_prefix "checkout"))
;; (defconst local_emacs_site_lisp_path (file-name-concat local_checkout_path "emacs-site-lisp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; require `load-path`
(load "core/bootstrap.el")

(init--load-modules '(
               ;; instead to comment several lines to stop at some point
               ;; use keyword
               ;;   :stop

               user-functions

               edition ; -> core ???

               ;; Completion
               completion/company
               completion/vertico
               ;; completion/previous ; buffer switch
               ;; code-completion/code-completion

               ;; UI
               ;; indent-guides
               ;; indent-guides
               ;; minimap
               ;; modeline
               ;; vc-gutter

               ;; Editor
               ;; multiple-cursors
               ;; snippets

               ;; Emacs
               emacs/dired
               emacs/electric
               ;; emacs/file-manager
               ;; undo

               ;; Checkers
               checkers/syntax
               checkers/spell
               checkers/grammar
               ;; checkers/checker

               ;; Tools
               tools/tree-sitter
               tools/magit
               tools/git

               ;; Lang
               lang/c-java
               lang/emacs-lisp
               lang/json
               lang/markdown
               lang/python
               lang/web
               lang/yaml
               ;; lang/lang
               ;; lang/javasctript

               sysadmin

               ;; finally
               post-keybinding
               ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom Settings

(setq custom-file (file-name-concat local_path_prefix "gnu-emacs-custom.el"))
(load custom-file t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Complete init startup

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1024 1024))

;;; init.el ends here

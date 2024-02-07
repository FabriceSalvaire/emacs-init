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

(defconst local_path_prefix "/home/common/emacs/") ; this is the only absolut path for Emacs files
(defconst local_checkout_path (concat local_path_prefix "checkout/"))
(defconst local_emacs_site_lisp_path (concat local_checkout_path "emacs-site-lisp/"))
(defconst local_emacs_d_path (concat local_path_prefix "lisp/"))
(defconst local_theme_path (concat local_path_prefix "themes/"))

(add-to-list 'load-path local_emacs_d_path)
(add-to-list 'load-path local_emacs_site_lisp_path)
(add-to-list 'custom-theme-load-path local_theme_path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun _load-files (&rest args)
  "Helper to load lisp files"
  (dolist (_ args)
    ;; (load FILE &optional NOERROR NOMESSAGE NOSUFFIX MUST-SUFFIX)
    (load (concat _ ".el") t t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(_load-files
 "doom-lib/doom-lib"
 "startup"

 "packages"
 "frame"
 "core"

 "user-functions"

 "completion" ; buffer switch
 "edition"
 "undo"
 "speller"

 "file-manager"
 "sysadmin"

 "tree-sitter"
 "code-completion"
 "lang/lang"
 "lang/c-java"
 "lang/python"
 "lang/web"
 "checker"
 "git"

 ;; must be after
 "keys"

 "server"
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom Settings

(setq custom-file (concat local_path_prefix "gnu-emacs-custom.el"))
(load custom-file t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1024 1024))

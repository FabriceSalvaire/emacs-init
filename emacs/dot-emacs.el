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
;;

(defconst local_path_prefix "/home/common/emacs/") ; this is the only absolut path for Emacs files
(defconst local_emacs_site_lisp_path (concat local_path_prefix "emacs-site-lisp/"))
(defconst local_emacs_d_path (concat local_path_prefix "emacs.d/"))
(defconst local_theme_path (concat local_emacs_d_path "themes/"))

(add-to-list 'load-path local_emacs_d_path)
(add-to-list 'load-path local_emacs_site_lisp_path)
(add-to-list 'custom-theme-load-path local_theme_path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Increase GC
;;   The default is 800 kilobytes.
;;   reseted at the end
;;   see also lsp config !
(setq gc-cons-threshold (* 100 1024 1024)) ; 100 MB

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load sub-config files
;;

(dolist (_
         '(
           "packages"
           "frame"
           "core"

           "user-functions"

           "file-manager"
           "completion" ; buffer switch
           "edition"
           "speller"

           "sysadmin"

           "tree-sitter"
           "code-completion"
           "lang/lang"
           "lang/c-java"
           "lang/python"
           "lang/web"
           "checker"

           ;; must be after
           "keys"
           )
         )
  ;; (load FILE &optional NOERROR NOMESSAGE NOSUFFIX MUST-SUFFIX)
  (load (concat _ ".el") t t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom Settings
;;

(setq custom-file (concat local_path_prefix "gnu-emacs-custom.el"))
(load custom-file t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Server
;;

;; Starts server for (among others) emacsclient
(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1024 1024))

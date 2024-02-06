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

(add-to-list 'load-path local_emacs_d_path)
(add-to-list 'load-path local_emacs_site_lisp_path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load sub-config files
;;

;; (load FILE &optional NOERROR NOMESSAGE NOSUFFIX MUST-SUFFIX)
(mapcar (lambda (_) (load (concat _ ".el") t t))
	'(
	  "packages"

	  "variables"
	  "user-functions"

	  "frame"
	  "font"
	  "behaviour"
	  "encoding"
	  "treemacs-settings"
	  "completion" ; buffer switch
	  "edition-settings"

	  "modes"
	  "sysadmin"

	  "programming"
	  "mode-languages"
	  "mode-c"
	  "mode-python"
	  "mode-web"

	  "spelling"
	  "flycheck-settings"

	  ;; must be after
	  "keys"

	  "server-settings"
	  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom Settings
;;

;; (setq custom-file (concat local_path_prefix "gnu-emacs-custom.el"))
;; (load custom-file t t)

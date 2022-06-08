;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                                                   ;
;                                              GNU-Emacs                                            ;
;                                                                                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; M-x load-file ~./emacs
;
; C-x C-f /path/to/... .el RET
; M-x byte-compile-file RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; To disable icicle warning
; (setq warning-minimum-level :emergency)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Path Settings
;

; expand-file-name


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;; This is the only absolut path for Emacs files
(setq local_path_prefix "/home/etc/users/emacs/")

(setq local_emacs_site_lisp_path (concat local_path_prefix "emacs-site-lisp/"))
(setq local_emacs_d_path (concat local_path_prefix "emacs.d/"))

(add-to-list 'load-path local_emacs_d_path)
(add-to-list 'load-path local_emacs_site_lisp_path)

; (byte-recompile-directory local_emacs_site_lisp_path 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Load sub-config files
;

(load "package-repository.el" t t)

(load "frame.el" t t)
(load "font.el" t t)
(load "behaviour.el" t t)
(load "completion.el" t t) ; buffer switch
(load "edition-settings.el" t t)

(load "modes.el" t t)
(load "sysadmin.el" t t)

(load "programming.el" t t)
(load "mode-languages.el" t t)
(load "mode-c.el" t t)
(load "mode-python.el" t t)
(load "mode-web.el" t t)

(load "spelling.el" t t)
(load "flycheck-settings.el" t t)

(load "user-functions.el" t t)

;; must be after
(load "keys.el" t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Custum Settings
;

(setq custom-file (concat local_path_prefix "gnu-emacs-custom.el"))
(load custom-file t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Load post-config files
;

(load "server-settings.el" t t)

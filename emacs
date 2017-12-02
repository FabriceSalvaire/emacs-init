; -*- lisp -*-

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
(package-initialize)

(setq local_path_prefix "/home/etc/users/")
(setq local_emacs_site_lisp_path (concat local_path_prefix "emacs-site-lisp/"))
(setq local_emacs_d_path (concat local_path_prefix "emacs.d/"))

(add-to-list 'load-path local_emacs_d_path)
(add-to-list 'load-path local_emacs_site_lisp_path)

; (byte-recompile-directory local_emacs_site_lisp_path 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Load sub-config files
;

(load "emacs-package-repository.el" t t)
(load "emacs-document-editing-modes.el" t t)
(load "emacs-edition-settings.el" t t)
(load "emacs-frame-font-settings.el" t t)
(load "emacs-key-bindings.el" t t)
(load "emacs-mode.el" t t)
(load "emacs-buffer-switch.el" t t)
(load "emacs-spelling.el" t t)
(load "emacs-sysadmin.el" t t)
; (load "emacs-helm.el" t t)

(load "emacs-programming.el" t t)
(load "emacs-programming-c.el" t t)
(load "emacs-programming-python.el" t t)
(load "emacs-programming-languages.el" t t)
(load "emacs-web.el" t t)
(load "emacs-flycheck.el" t t)

(load "emacs-user-functions.el" t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Custum Settings
;

(setq custom-file (concat local_path_prefix "gnu-emacs-custom"))
(load custom-file t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Load post-config files
;

(load "emacs-server.el" t t)

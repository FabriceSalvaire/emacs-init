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
;
; Path Settings
;

; expand-file-name
(setq local_path_prefix "/home/etc/users/")
(setq local_emacs_site_lisp_path (concat local_path_prefix "emacs-site-lisp/"))
(setq local_emacs_d_path (concat local_path_prefix "emacs.d/"))

(add-to-list 'load-path local_emacs_d_path)
(add-to-list 'load-path local_emacs_site_lisp_path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Load sub-config files
;

(load "emacs-document-editing-modes.el" t t)
(load "emacs-edition-settings.el" t t)
(load "emacs-frame-font-settings.el" t t)
(load "emacs-key-bindings.el" t t)
(load "emacs-mode.el" t t)
(load "emacs-spelling.el" t t)
(load "emacs-sysadmin.el" t t)

(load "emacs-programming.el" t t)
(load "emacs-flymake.el" t t)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; End
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

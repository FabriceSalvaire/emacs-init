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
; Path
;

(add-to-list 'load-path (expand-file-name "/home/etc/users/emacs-site-lisp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Frame settings
;

;(speedbar t)

; Disable the tool bar
(tool-bar-mode 0)

; Menu for recently opened files
(recentf-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Set Font
;

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(defun initialise_font ()
  (interactive)
  (if window-system
      (progn
;        (setq initial-frame-alist '((top    . 1)
;                                   (left   . 1)
;                                   (width  . 200)
;                                   (height . 200)))
;       (set-default-font "-adobe-courier-medium-r-normal-*-*-140-*-*-m-*-iso8859-15")
;       (set-default-font "-jis-fixed-medium-r-normal--16-110-100-100-c-160-jisx0208.1983-0")
        (set-foreground-color "wheat")
        (set-background-color "black")
;       Set the line spacing
        (setq-default line-spacing 3) ; px
;       (setq-default line-spacing .15) ; %
        )))

(initialise_font )  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Key Bindings
;

(global-set-key [delete] 'delete-char) 

(global-set-key [C-up]   'ignore) ; was backward-paragraph
(global-set-key [C-down] 'ignore) ; was forward-paragraph
 
(global-set-key [C-prior] 'ignore) ; was scroll-right
(global-set-key [C-next]  'ignore) ; was scroll-left

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Mule
;

; (set-keyboard-coding-system 'mule-utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Fill-column & Auto fill mode
;

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq fill-column 100)

(add-hook 'TeX-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'outline-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'python-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'text-mode-hook '(lambda () (setq fill-column 80)))

(setq column-number-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Server
;

(server-start) ; Starts server for (among others) emacsclient

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Load
;

(load "/home/etc/users/emacs-site-lisp/emacs-developments.el" t t)
(load "/home/etc/users/emacs-site-lisp/emacs-modes.el" t t)
(load "/home/etc/users/emacs-site-lisp/emacs-functions.el" t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Custum Settings
;

(setq custom-file "/home/etc/users/gnu-emacs-custom")
(load "/home/etc/users/gnu-emacs-custom" t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; End
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

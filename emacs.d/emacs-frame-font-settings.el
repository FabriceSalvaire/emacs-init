;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Frame settings
;

;(speedbar t)

; Disable the tool bar
(tool-bar-mode nil)

; Menu for recently opened files
(recentf-mode t)

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
;				    (left   . 1)
;				    (width  . 200)
;				    (height . 200)))

;	(set-foreground-color "wheat")
;	(set-background-color "black")

	(set-foreground-color "black")
	(set-background-color "white")

;;      Set the line spacing
	(setq-default line-spacing 3) ; px
;       (setq-default line-spacing .15) ; %
	)))

(initialise_font)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; End
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

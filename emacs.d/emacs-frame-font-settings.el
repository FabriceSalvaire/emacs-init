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
;				    (left   . 1)
;				    (width  . 200)
;				    (height . 200)))
;       (set-default-font "-adobe-courier-medium-r-normal-*-*-140-*-*-m-*-iso8859-15")
;	(set-default-font "-jis-fixed-medium-r-normal--16-110-100-100-c-160-jisx0208.1983-0")
;	(set-foreground-color "wheat")
;	(set-background-color "black")
	(set-foreground-color "black")
	(set-background-color "white")
;       Set the line spacing
	(setq-default line-spacing 3) ; px
;       (setq-default line-spacing .15) ; %
	)))

(initialise_font )  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; End
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

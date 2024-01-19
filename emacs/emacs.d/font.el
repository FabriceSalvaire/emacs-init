;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set Font
;;

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; '(default ((t
;;             (:stipple nil
;;              :background "white" :foreground "black"
;;              :inverse-video nil :box nil :strike-through nil
;;              :overline nil :underline nil :slant normal :weight normal :height 143 :width normal
;;              :foundry "PfEd" :family "DejaVu Sans Mono"
;;              ))))

;; (defun set_font_normal ()
;;   (interactive)
;;   (set-frame-font "DejaVu Sans Mono 14")
;;   )

;; (defun set_font_14 ()
;;   (interactive)
;;   (set-frame-font "Roboto Mono 14")
;;   )

;; (defun set_font_13 ()
;;   (interactive)
;;   (set-frame-font "Roboto Mono 13")
;;   )

;; (defun set_font_12 ()
;;   (interactive)
;;   (set-frame-font "Roboto Mono 12")
;;   )

;; (defun set_font_11 ()
;;   (interactive)
;;   (set-frame-font "Roboto Mono 11")
;;   )

;; (defun set_font_10 ()
;;   (interactive)
;;   (set-frame-font "Roboto Mono 10")
;;   )

(defun initialise_font ()
  (interactive)
  (if window-system
      (progn
	;; (setq initial-frame-alist '((top    . 1)
	;; 				    (left   . 1)
	;; 				    (width  . 200)
	;; 				    (height . 200)))

	;; (set-foreground-color "wheat")
	;; (set-background-color "black")

	(set-foreground-color "black")
	(set-background-color "white")

	;; Set the line spacing
	(setq-default line-spacing 3) ; px
	;; (setq-default line-spacing .15) ; %

	;; see custom-set-variables
	;; (set_font_normal)
	)))

(initialise_font)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Clean Indentation Mode
;;

(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Column Marker

;; python-mode.el-6.0.4/tools/column-marker.el
(require 'column-marker)

;; Highlight column 100 in python mode.
(add-hook 'python-mode-hook '(lambda () (interactive) (column-marker-1 100)))

;; (add-hook 'sass-mode-hook '(lambda ()
;; 			     (interactive)
;; 			     (column-marker-1 4)
;; 			     (column-marker-2 8)
;; 			     (column-marker-3 12)
;; 			     ))

;; Use `C-c m' interactively to highlight with face `column-marker-1'.
;; (global-set-key [?\C-c ?m] 'column-marker-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Fill-column & Auto fill mode
;;

; (add-hook 'text-mode-hook 'turn-on-auto-fill)

; SET AS CUSTOM ELSE NOT SET !!!
(setq fill-column 100)

(add-hook 'TeX-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'c-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'cmake-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'django-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'glsl-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'outline-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'python-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'rst-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'sass-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'text-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'web-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'yaml-mode-hook '(lambda () (setq fill-column 100)))

(setq column-number-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Highlight Indentation

;; https://www.emacswiki.org/emacs/HighlightEndOfBuffer
(setq-default indicate-empty-lines t)

;; python-mode.el-6.0.4/highlight-indentation.el
(require 'highlight-indentation)

;; https://www.emacswiki.org/emacs/ShowWhiteSpace
;; https://github.com/Lindydancer/char-font-lock

;; https://www.emacswiki.org/emacs/WhiteSpace
;; (require 'whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Indent Guide
;;

(require 'indent-guide)
;; (indent-guide-global-mode)
;; (set-face-background 'indent-guide-face "dimgray")
;; (setq indent-guide-delay 0.1)
;; (setq indent-guide-recursive t)
;; (setq indent-guide-char ":")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Line Number

; (require 'linum)
; (global-linum-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Indent Tabs Mode
;

; (setq-default 'indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Mule
;

;(set-keyboard-coding-system 'mule-utf-8)

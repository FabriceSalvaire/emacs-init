;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Clean Indentation Mode
;;  Emacs extension for clean auto-indent and backspace unindent
;;  https://github.com/pmarinov/clean-aindent-mode last 2015

(use-package clean-aindent-mode
  :hook
  (prog-mode . clean-aindent-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Trailing Whitespace
;;   https://stackoverflow.com/questions/34531831/highlighting-trailing-whitespace-in-emacs-without-changing-character

(setq-default show-trailing-whitespace t)
(set-face-attribute 'trailing-whitespace nil :background "gray") ; was red1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Column Marker
;; python-mode.el-6.0.4/tools/column-marker.el

(use-package column-marker
  :init
  ;; Highlight column 100 in python mode.
  (add-hook 'python-mode-hook '(lambda () (interactive) (column-marker-1 100)))
  ;; (add-hook 'sass-mode-hook '(lambda ()
  ;;                            (interactive)
  ;;                            (column-marker-1 4)
  ;;                            (column-marker-2 8)
  ;;                            (column-marker-3 12)
  ;;                            ))
  ;; :hook (python-mode . (...)))
  )

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
(use-package highlight-indentation)

;; https://www.emacswiki.org/emacs/ShowWhiteSpace
;; https://github.com/Lindydancer/char-font-lock

;; https://www.emacswiki.org/emacs/WhiteSpace
;; (require 'whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Indent Guide
;;

(use-package indent-guide)
;; (indent-guide-global-mode)
;; (set-face-background 'indent-guide-face "dimgray")
;; (setq indent-guide-delay 0.1)
;; (setq indent-guide-recursive t)
;; (setq indent-guide-char ":")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; String Inflection
;;

; https://github.com/akicho8/string-inflection
(use-package string-inflection)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Line Number

; (use-package linum)
; (global-linum-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Indent Tabs Mode
;

; (setq-default 'indent-tabs-mode nil)

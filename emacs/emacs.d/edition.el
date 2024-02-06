(use-package emacs
  :config

  ;; Indent Tabs Mode
  ;; (setq-default 'indent-tabs-mode nil)

  ;; Trailing Whitespace
  ;;   https://stackoverflow.com/questions/34531831/highlighting-trailing-whitespace-in-emacs-without-changing-character
  (setq-default show-trailing-whitespace t)
  (set-face-attribute 'trailing-whitespace nil :background "gray") ; was red1

  ;; Empty Line
  ;; https://www.emacswiki.org/emacs/HighlightEndOfBuffer
  (setq-default indicate-empty-lines t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Fill-column & Auto fill mode
  ;;
  (setq column-number-mode t)
  (dolist (_
	   '(
	     TeX-mode-hook
	     c-mode-hook
	     cmake-mode-hook
	     django-mode-hook
	     emacs-lisp-mode-hook
	     glsl-mode-hook
	     outline-mode-hook
	     python-mode-hook
	     rst-mode-hook
	     sass-mode-hook
	     text-mode-hook
	     web-mode-hook
	     yaml-mode-hook
	     )
	   )
    (add-hook _ '(lambda () (setq fill-column 100)))
    )
  ;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
  ;; SET AS CUSTOM ELSE NOT SET !!!
  ;; (setq fill-column 100)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Multiple cursors
;;   https://github.com/magnars/multiple-cursors.el

(use-package multiple-cursors
  :ensure t
  ;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  ;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  ;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  ;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Column Marker
;;   python-mode.el-6.0.4/tools/column-marker.el

(use-package column-marker
  :config
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Highlight Indentation

;; python-mode.el-6.0.4/highlight-indentation.el
(use-package highlight-indentation
  :disabled
  )

;; https://www.emacswiki.org/emacs/ShowWhiteSpace
;; https://github.com/Lindydancer/char-font-lock

;; https://www.emacswiki.org/emacs/WhiteSpace
;; (require 'whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Indent Guide
;;   https://github.com/zk-phi/indent-guide

(use-package indent-guide
  :disabled
  ;; (indent-guide-global-mode)
  ;; (set-face-background 'indent-guide-face "dimgray")
  ;; (setq indent-guide-delay 0.1)
  ;; (setq indent-guide-recursive t)
  ;; (setq indent-guide-char ":")
  )

;; Fixme: !!!
;; https://github.com/DarthFennec/highlight-indent-guides

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; show-paren-mode — highlight matching delimiters
;;   https://www.emacswiki.org/emacs/ShowParenMode

(use-package paren
  :custom
  (show-paren-delay 0.1)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)

  :init
  ;; global
  (show-paren-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; String Inflection
;;    underscore -> UPCASE -> CamelCase conversion of names
;;    https://github.com/akicho8/string-inflection

(use-package string-inflection
  :defer t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Clean Indentation Mode
;;  Emacs extension for clean auto-indent and backspace unindent
;;  https://github.com/pmarinov/clean-aindent-mode last 2015

(use-package clean-aindent-mode
  :disabled
  :hook
  (prog-mode . clean-aindent-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Line Number

(use-package linum
  :disabled
  :init
  (global-linum-mode 1)
  )

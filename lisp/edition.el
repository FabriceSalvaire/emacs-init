;;; .el --- ... -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://www.emacswiki.org/emacs/ShowWhiteSpace
;; https://github.com/Lindydancer/char-font-lock

;; See Doom doom-editor.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Indent Tabs Mode

(setq-default indent-tabs-mode nil)
(dolist (_
         '(
           c-mode-hook
           cmake-mode-hook
           emacs-lisp-mode-hook
           )
         )
  (add-hook _ '(lambda () (setq indent-tabs-mode nil)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Line & Column

(setq column-number-mode t)
;; (setq line-number-mode t)
;; (global-hl-line-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Fill-column & Auto fill mode

;; SET AS CUSTOM ELSE NOT SET !!!
;; (setq fill-column 100)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Whitespace
;;   https://www.emacswiki.org/emacs/WhiteSpace
;;   https://github.com/VernonGrant/discovering-emacs/blob/main/show-notes/4-using-whitespace-mode.md

(use-package whitespace
  :hook (prog-mode . whitespace-mode)

  :config
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Trailing Whitespace
  ;;   https://stackoverflow.com/questions/34531831/highlighting-trailing-whitespace-in-emacs-without-changing-character
  ;; (setq-default show-trailing-whitespace t)
  ;; (set-face-attribute 'trailing-whitespace nil :background "gray") ; was red1

  ;; Empty Line
  ;; https://www.emacswiki.org/emacs/HighlightEndOfBuffer
  ;; (setq-default indicate-empty-lines t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq-default whitespace-style
                '(
                  face ; visualize by using faces
                  trailing ; visualize trailing blanks via faces
                  tabs ; visualize TABs via faces.
                  ;; spaces ; visualize SPACEs and HARD SPACEs via faces.
                  ;; lines ; highlight lines which have columns beyond ‘whitespace-line-column’ via faces
                  ;; lines-tail ; tail variant
                  ;; lines-char ; char variant
                  ;; newline ; visualize NEWLINEs via faces
                  ;; missing-newline-at-eof ; visualize missing newline at the end of the file via faces
                  empty ; visualize empty lines at beginning and/or end of buffer via faces.
                  ;; indentation::tab ; visualize ‘tab-width’ or more SPACEs at beginning of line via faces
                  ;; indentation::space ; visualize TABs at beginning of line via faces.
                  ;; indentation ; visualize ‘tab-width’ or more SPACEs at beginning of line, if
                                 ; ‘indent-tabs-mode’ (which see) is non-nil; otherwise, visualize TABs
                                 ; at beginning of line via faces.
                  ;; big-indent ; visualize big indentations via faces.
                  ;; space-after-tab::tab ; visualize ‘tab-width’ or more SPACEs after a TAB via faces.
                  ;; space-after-tab::space ; visualize TABs when ‘tab-width’
                                           ; or more SPACEs occur after a TAB, via faces.
                  ;;space-after-tab ; visualize ‘tab-width’ or more SPACEs after a TAB, if
                                    ; ‘indent-tabs-mode’ (which see) is non-nil; otherwise,visualize
                                    ; the TABs via faces.
                  ;; space-before-tab::tab ; visualize SPACEs before TAB via faces.
                  ;; space-before-tab::space ; visualize TABs when SPACEs occur before TAB, via faces.
                  ;; space-before-tab ; visualize SPACEs before TAB, if ‘indent-tabs-mode’ (which see)
                                      ; is non-nil; otherwise, visualize TABs via faces.

                  ; space-mark ; visualize SPACEs and HARD SPACEs via display table.
                  ; tab-mark ; visualize TABs via display table.
                  ; newline-mark ; visualize NEWLINEs via display table.
                  ))

  ;; Fixme: see theme
  (require 'color)
  (let* ((fg "#ee0000")
         (bg (color-darken-name "#ffffff" 40))
         )
    (custom-set-faces
     `(whitespace-trailing               ((t (:background ,bg :foreground ,fg))))
     `(whitespace-tab                    ((t (:background ,bg :foreground ,fg))))
     `(whitespace-empty                  ((t (:background ,bg :foreground ,fg))))
     ;; `(whitespace-newline                ((t (:background ,bg :foreground ,fg))))
     ;; `(whitespace-missing-newline-at-eof ((t (:background ,bg :foreground ,fg))))
     ;; `(whitespace-space                  ((t (:background ,bg :foreground ,fg))))
     ;; `(whitespace-space-after-tab        ((t (:background ,bg :foreground ,fg))))
     ;; `(whitespace-space-before-tab       ((t (:background ,bg :foreground ,fg))))
    ))

  ;; (KIND CHAR VECTOR...)
  ;; The first vector that can be displayed by the terminal is used
  (setq-default whitespace-display-mappings
                '((tab-mark     ?\t [?› ?\t])
                  (newline-mark ?\n [?¬ ?\n])
                  (space-mark   ?\  [?·] [?.])))

  ;; Don't enable whitespace for.
  ;; (setq-default whitespace-global-modes
  ;;               '(not shell-mode
  ;;                     help-mode
  ;;                     magit-mode
  ;;                     magit-diff-mode
  ;;                     ibuffer-mode
  ;;                     dired-mode
  ;;                     occur-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Multiple cursors
;;   https://github.com/magnars/multiple-cursors.el

(use-package multiple-cursors
  ;;s;; :ensure t
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
  ;; Highlight column 100 in python mode.
  :hook (python-mode . (lambda () (interactive) (column-marker-1 100)))

  :config
  ;; (add-hook 'sass-mode-hook '(lambda ()
  ;;                            (interactive)
  ;;                            (column-marker-1 4)
  ;;                            (column-marker-2 8)
  ;;                            (column-marker-3 12)
  ;;                            ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Highlight Indentation
;;   https://github.com/DarthFennec/highlight-indent-guides

;; Fixme: only with character/bitmap
;;   issue when we comment or copy several lines
;;   e.g. we got "; | ..."
;;   for example, it happens when the first line is not indented
;;   https://github.com/DarthFennec/highlight-indent-guides/issues/77

(use-package highlight-indent-guides
  ;; :disabled
  :ensure nil
  :load-path (lambda () (file-name-concat local_checkout_path "highlight-indent-guides"))
  :hook (prog-mode . highlight-indent-guides-mode)

  :custom
  (highlight-indent-guides-auto-odd-face-perc 20)
  (highlight-indent-guides-auto-even-face-perc 30)
  (highlight-indent-guides-auto-character-face-perc 80)
  (highlight-indent-guides-auto-enabled t)
  ;; (highlight-indent-guides-method 'fill)
  (highlight-indent-guides-method 'column)
  ;; (highlight-indent-guides-method 'character)
  ;; (highlight-indent-guides-method 'bitmap)
  ;; See theme
  ;; (highlight-indent-guides-character-face ((t (:foreground "gainsboro"))))
  ;; (highlight-indent-guides-stack-character-face ((t (:inherit (highlight-indent-guides-character-face)))))
  ;; (highlight-indent-guides-top-character-face ((t (:inherit (highlight-indent-guides-character-face)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Highlight Indentation
;;  https://github.com/antonj/Highlight-Indentation-for-Emacs
;;  python-mode.el-6.0.4/highlight-indentation.el

(use-package highlight-indentation
  :disabled
  )

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; show-paren-mode — highlight matching delimiters
;;   https://www.emacswiki.org/emacs/ShowParenMode

(use-package paren
  :hook (doom-first-buffer . show-paren-mode)
  :custom
  (show-paren-delay 0.1)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
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

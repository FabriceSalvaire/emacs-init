;;; .el --- ... -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Edition
;;   See Doom doom-editor.el
;;   - fill-colum / column marker
;;   - long line
;;   - indentation / whitespace
;;   - parenthese
;;   - sentence
;;   - EOL
;;   - clipboard / kill-ring
;;   - better-jump
;;   - multiple cursor
;;   - string inflection
;;   - minified file
;;
;; https://www.emacswiki.org/emacs/ShowWhiteSpace
;; https://github.com/Lindydancer/char-font-lock
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Column

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (add-hook _ #'(lambda () (setq fill-column 100)))
  )

;; (add-hook 'text-mode-hook 'turn-on-auto-fill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Column Marker
;;   https://www.emacswiki.org/emacs/ColumnMarker
;;   python-mode.el-6.0.4/tools/column-marker.el

(use-package column-marker
  ;; Highlight column 100 in python mode.
  :hook
  ((prog-mode) . (lambda () (interactive) (column-marker-1 100)))
  ;; :hook ((python-mode python-ts-mode) . (lambda () (interactive) (column-marker-1 100)))

  :config
  ;; (add-hook 'sass-mode-hook '(lambda ()
  ;;                            (interactive)x
  ;;                            (column-marker-1 4)
  ;;                            (column-marker-2 8)
  ;;                            (column-marker-3 12)
  ;;                            ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Long Line

;; Continue wrapped words at whitespace, rather than in the middle of a word.
(setq-default word-wrap t)
;; ...but don't do any wrapping by default. It's expensive. Enable
;; `visual-line-mode' if you want soft line-wrapping. `auto-fill-mode' for hard
;; line-wrapping.
(setq-default truncate-lines t)
;; If enabled (and `truncate-lines' was disabled), soft wrapping no longer
;; occurs when that window is less than `truncate-partial-width-windows'
;; characters wide. We don't need this, and it's extra work for Emacs otherwise,
;; so off it goes.
(setq truncate-partial-width-windows nil)

;; Default to soft line-wrapping in text modes. It is more sensibile for text
;; modes, even if hard wrapping is more performant.
(add-hook 'text-mode-hook #'visual-line-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Indentation / Whitespace

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tab

;; Favor spaces over tabs
(setq-default indent-tabs-mode nil
              tab-width 4)

;; (dolist (_
;;          '(
;;            c-mode-hook
;;            cmake-mode-hook
;;            emacs-lisp-mode-hook
;;            )
;;          )
;;   (add-hook _ #'(lambda () (setq indent-tabs-mode nil)))
;;   )

;; Only indent the line when at BOL or in a line's indentation.
;; Anywhere else, insert literal indentation.
;; (setq-default tab-always-indent nil)

;; Make `tabify' and `untabify' only affect indentation.
;; Not tabs/spaces in the middle of a line.
(setq tabify-regexp "^\t* [ \t]+")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  dtrt-indent
;;    An Emacs minor mode that guesses the indentation offset originally used for creating source
;;    code files and transparently adjusts the corresponding settings in Emacs, making it more
;;    convenient to edit foreign files.
;;    https://github.com/jscheid/dtrt-indent

;; Fixme: move ? <<<
(defvar doom-detect-indentation-excluded-modes
  '(fundamental-mode pascal-mode so-long-mode doom-docs-org-mode)
  "A list of major modes in which indentation should be automatically
detected.")

(defvar-local doom-inhibit-indent-detection nil
  "A buffer-local flag that indicates whether `dtrt-indent' should try to detect
indentation settings or not. This should be set by editorconfig if it
successfully sets indent_style/indent_size.")

(defvar doom-inhibit-large-file-detection nil
  "If non-nil, inhibit large/long file detection when opening files.")

(defvar doom-large-file-p nil)
(put 'doom-large-file-p 'permanent-local t)
;; Fixme: move ? >>>

(use-package dtrt-indent
  ;; Automatic detection of indent settings
  :unless noninteractive
  ;; I'm not using `global-dtrt-indent-mode' because it has hard-coded and rigid
  ;; major mode checks, so I implement it in `doom-detect-indentation-h'.
  :hook ((change-major-mode-after-body read-only-mode) . doom-detect-indentation-h)

  :config
  (defun doom-detect-indentation-h ()
    (unless (or (not after-init-time)
                doom-inhibit-indent-detection
                doom-large-file-p
                (memq major-mode doom-detect-indentation-excluded-modes)
                (member (substring (buffer-name) 0 1) '(" " "*")))
      ;; Don't display messages in the echo area, but still log them
      (let ((inhibit-message (not init-file-debug)))
        (dtrt-indent-mode +1))))

  ;; Enable dtrt-indent even in smie modes so that it can update `tab-width',
  ;; `standard-indent' and `evil-shift-width' there as well.
  (setq dtrt-indent-run-after-smie t)
  ;; Reduced from the default of 5000 for slightly faster analysis
  (setq dtrt-indent-max-lines 2000)

  ;; always keep tab-width up-to-date
  (push '(t tab-width) dtrt-indent-hook-generic-mapping-list)

  (defvar dtrt-indent-run-after-smie)
  (defadvice! doom--fix-broken-smie-modes-a (fn &optional arg)
    "Some smie modes throw errors when trying to guess their indentation, like
`nim-mode'. This prevents them from leaving Emacs in a broken state."
    :around #'dtrt-indent-mode
    (let ((dtrt-indent-run-after-smie dtrt-indent-run-after-smie))
      (letf! ((defun symbol-config--guess (beg end)
                (funcall symbol-config--guess beg (min end 10000)))
              (defun smie-config-guess ()
                (condition-case e (funcall smie-config-guess)
                  (error (setq dtrt-indent-run-after-smie t)
                         (message "[WARNING] Indent detection: %s"
                                  (error-message-string e))
                         (message ""))))) ; warn silently
        (funcall fn arg))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Whitespace
;;   https://www.emacswiki.org/emacs/WhiteSpace
;;   https://github.com/VernonGrant/discovering-emacs/blob/main/show-notes/4-using-whitespace-mode.md
;;   see doom-ui.el

(use-package whitespace
  :hook (prog-mode . whitespace-mode)

  :config
  ;; Trailing Whitespace
  ;;   https://stackoverflow.com/questions/34531831/highlighting-trailing-whitespace-in-emacs-without-changing-character
  ;; (setq-default show-trailing-whitespace t)
  ;; (set-face-attribute 'trailing-whitespace nil :background "gray") ; was red1

  ;; Empty Line
  ;; https://www.emacswiki.org/emacs/HighlightEndOfBuffer
  ;; (setq-default indicate-empty-lines t)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ws-butler — an unobtrusive way to trim spaces from end of line
;;   Only lines touched get trimmed.
;;   If the white space at end of buffer is changed, then blank lines at the end of buffer are
;;   truncated respecting require-final-newline.
;;   Trimming only happens when saving
;;   https://github.com/lewang/ws-butler

(use-package ws-butler
  ;; a less intrusive `delete-trailing-whitespaces' on save
  :hook (doom-first-buffer . ws-butler-global-mode)

  :config
  ;; ws-butler normally preserves whitespace in the buffer (but strips it from the written
  ;; file). While sometimes convenient, this behavior is not intuitive. To the average user it looks
  ;; like whitespace cleanup is failing, which causes folks to redundantly install their own.
  (setq ws-butler-keep-whitespace-before-point nil)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Clean Indentation Mode
;;  Emacs extension for clean auto-indent and backspace unindent
;;  https://github.com/pmarinov/clean-aindent-mode last 2015

;; (use-package clean-aindent-mode
;;   :disabled
;;   :hook
;;   (prog-mode . clean-aindent-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  ;; :ensure nil
  ;; :load-path (lambda () (file-name-concat local_checkout_path "highlight-indent-guides"))
  :straight (highlight-indent-guides :type git :host github :repo "DarthFennec/highlight-indent-guides")
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Highlight Indentation
;;  https://github.com/antonj/Highlight-Indentation-for-Emacs
;;  python-mode.el-6.0.4/highlight-indentation.el

;; (use-package highlight-indentation
;;   :disabled
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Indent Guide
;;   https://github.com/zk-phi/indent-guide

;; (use-package indent-guide
;;   :disabled
;;   ;; (indent-guide-global-mode)
;;   ;; (set-face-background 'indent-guide-face "dimgray")
;;   ;; (setq indent-guide-delay 0.1)
;;   ;; (setq indent-guide-recursive t)
;;   ;; (setq indent-guide-char ":")
;;   )

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Smartparens — a minor mode for dealing with pairs in Emacs.
;;   https://github.com/Fuco1/smartparens

(use-package smartparens
  ;; Auto-close delimiters and blocks as you type. It's more powerful than that,
  ;; but that is all Doom uses it for.
  :hook (doom-first-buffer . smartparens-global-mode)
  :commands
  sp-pair
  sp-local-pair
  sp-with-modes
  sp-point-in-comment
  sp-point-in-string

  :config
  ;; Fixme: doom....
  ;; (add-to-list 'doom-point-in-string-functions 'sp-point-in-string)
  ;; (add-to-list 'doom-point-in-comment-functions 'sp-point-in-comment)
  ;; smartparens recognizes `slime-mrepl-mode', but not `sly-mrepl-mode', so...
  (add-to-list 'sp-lisp-modes 'sly-mrepl-mode)
  ;; Load default smartparens rules for various languages
  (require 'smartparens-config)
  ;; Overlays are too distracting and not terribly helpful. show-parens does
  ;; this for us already (and is faster), so...
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  ;; (with-eval-after-load 'evil
  ;;   ;; But if someone does want overlays enabled, evil users will be stricken
  ;;   ;; with an off-by-one issue where smartparens assumes you're outside the
  ;;   ;; pair when you're really at the last character in insert mode. We must
  ;;   ;; correct this vile injustice.
  ;;   (setq sp-show-pair-from-inside t)
  ;;   ;; ...and stay highlighted until we've truly escaped the pair!
  ;;   (setq sp-cancel-autoskip-on-backward-movement nil)
  ;;   ;; Smartparens conditional binds a key to C-g when sp overlays are active
  ;;   ;; (even if they're invisible). This disruptively changes the behavior of
  ;;   ;; C-g in insert mode, requiring two presses of the key to exit insert mode.
  ;;   ;; I don't see the point of this keybind, so...
  ;;   (setq sp-pair-overlay-keymap (make-sparse-keymap)))

  ;; The default is 100, because smartparen's scans are relatively expensive
  ;; (especially with large pair lists for some modes), we reduce it, as a
  ;; better compromise between performance and accuracy.
  (setq sp-max-prefix-length 25)
  ;; No pair has any business being longer than 4 characters; if they must, set
  ;; it buffer-locally. It's less work for smartparens.
  (setq sp-max-pair-length 4)

  ;; Silence some harmless but annoying echo-area spam
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (alist-get key sp-message-alist) nil))

  (add-hook! 'eval-expression-minibuffer-setup-hook
    (defun doom-init-smartparens-in-eval-expression-h ()
      "Enable `smartparens-mode' in the minibuffer for `eval-expression'.
This includes everything that calls `read--expression', e.g.
`edebug-eval-expression' Only enable it if
`smartparens-global-mode' is on."
      (when smartparens-global-mode (smartparens-mode +1))))
  (add-hook! 'minibuffer-setup-hook
    (defun doom-init-smartparens-in-minibuffer-maybe-h ()
      "Enable `smartparens' for non-`eval-expression' commands.
Only enable `smartparens-mode' if `smartparens-global-mode' is
on."
      (when (and smartparens-global-mode (memq this-command '(evil-ex)))
        (smartparens-mode +1))))

  ;; You're likely writing lisp in the minibuffer, therefore, disable these
  ;; quote pairs, which lisps doesn't use for strings:
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "'" nil :actions nil)
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "`" nil :actions nil)

  ;; ;; Smartparens breaks evil-mode's replace state
  ;; (defvar doom-buffer-smartparens-mode nil)
  ;; (add-hook! 'evil-replace-state-exit-hook
  ;;   (defun doom-enable-smartparens-mode-maybe-h ()
  ;;     (when doom-buffer-smartparens-mode
  ;;       (turn-on-smartparens-mode)
  ;;       (kill-local-variable 'doom-buffer-smartparens-mode))))
  ;; (add-hook! 'evil-replace-state-entry-hook
  ;;   (defun doom-disable-smartparens-mode-maybe-h ()
  ;;     (when smartparens-mode
  ;;       (setq-local doom-buffer-smartparens-mode t)
  ;;       (turn-off-smartparens-mode))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Sentence

;; (setq sentence-end-double-space nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EOL
;;
;; The POSIX standard defines a line is "a sequence of zero or more non-newline characters followed
;; by a terminating newline", so files should end in a newline.
;; Windows doesn't respect this (because it's Windows), but we should, since programmers' tools tend
;; to be POSIX compliant (and no big deal if not).

(setq require-final-newline t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Clipboard / kill-ring

;; Cull duplicates in the kill ring to reduce bloat and make the kill ring easier to peruse
;; (with `counsel-yank-pop' or `helm-show-kill-ring'.
(setq kill-do-not-save-duplicates t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; better-jumper
;;   A configurable jump list implementation for Emacs that can be used to easily jump back to
;;   previous locations.
;;   https://github.com/gilbertw1/better-jumper

(use-package better-jumper
  :hook (doom-first-input . better-jumper-mode)
  :commands
  doom-set-jump-a
  doom-set-jump-maybe-a
  doom-set-jump-h

  ;; :preface
  ;; ;; REVIEW Suppress byte-compiler warning spawning a *Compile-Log* buffer at
  ;; ;; startup. This can be removed once gilbertw1/better-jumper#2 is merged.
  ;; (defvar better-jumper-local-mode nil)

  :init
  (global-set-key [remap evil-jump-forward]  #'better-jumper-jump-forward)
  (global-set-key [remap evil-jump-backward] #'better-jumper-jump-backward)
  (global-set-key [remap xref-pop-marker-stack] #'better-jumper-jump-backward)
  (global-set-key [remap xref-go-back] #'better-jumper-jump-backward)
  (global-set-key [remap xref-go-forward] #'better-jumper-jump-forward)

  :config
  (defun doom-set-jump-a (fn &rest args)
    "Set a jump point and ensure fn doesn't set any new jump points."
    (better-jumper-set-jump (if (markerp (car args)) (car args)))
    (let ((evil--jumps-jumping t)
          (better-jumper--jumping t))
      (apply fn args)))

  (defun doom-set-jump-maybe-a (fn &rest args)
    "Set a jump point if fn actually moves the point."
    (let ((origin (point-marker))
          (result
           (let* ((evil--jumps-jumping t)
                  (better-jumper--jumping t))
             (apply fn args)))
          (dest (point-marker)))
      (unless (equal origin dest)
        (with-current-buffer (marker-buffer origin)
          (better-jumper-set-jump
           (if (markerp (car args))
               (car args)
             origin))))
      (set-marker origin nil)
      (set-marker dest nil)
      result))

  (defun doom-set-jump-h ()
    "Run `better-jumper-set-jump' but return nil, for short-circuiting hooks."
    (better-jumper-set-jump)
    nil)

  ;; Creates a jump point before killing a buffer. This allows you to undo
  ;; killing a buffer easily (only works with file buffers though; it's not
  ;; possible to resurrect special buffers).
  ;;
  ;; I'm not advising `kill-buffer' because I only want this to affect
  ;; interactively killed buffers.
  (advice-add #'kill-current-buffer :around #'doom-set-jump-a)

  ;; Create a jump point before jumping with imenu.
  (advice-add #'imenu :around #'doom-set-jump-a)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; String Inflection
;;    underscore -> UPCASE -> CamelCase conversion of names
;;    https://github.com/akicho8/string-inflection

(use-package string-inflection
  :defer t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; so-long
;;   This is a workaround for the issue of large ‘minified’ files of programming code
;;   https://www.emacswiki.org/emacs/SoLong

(use-package so-long
  :hook (doom-first-file . global-so-long-mode)

  :config
  ;; Emacs 29 introduced faster long-line detection, so they can afford a much
  ;; larger `so-long-threshold' and its default `so-long-predicate'.
  ;; Fixme: comment ?
  (if (fboundp 'buffer-line-statistics)
      (unless (featurep 'native-compile)
        (setq so-long-threshold 5000))
    ;; reduce false positives w/ larger threshold
    (setq so-long-threshold 400)

    (defun doom-buffer-has-long-lines-p ()
      (unless (bound-and-true-p visual-line-mode)
        (let ((so-long-skip-leading-comments
               ;; HACK Fix #2183: `so-long-detected-long-line-p' calls
               ;;   `comment-forward' which tries to use comment syntax, which
               ;;   throws an error if comment state isn't initialized, leading
               ;;   to a wrong-type-argument: stringp error.
               ;; DEPRECATED Fixed in Emacs 28.
               (bound-and-true-p comment-use-syntax)))
          (so-long-detected-long-line-p))))
    (setq so-long-predicate #'doom-buffer-has-long-lines-p))

  ;; Don't disable syntax highlighting and line numbers, or make the buffer read-only, in
  ;; `so-long-minor-mode', so we can have a basic editing experience in them, at least. It will
  ;; remain off in `so-long-mode', however, because long files have a far bigger impact on Emacs
  ;; performance.
  (delq! 'font-lock-mode so-long-minor-modes)
  (delq! 'display-line-numbers-mode so-long-minor-modes)
  (delq! 'buffer-read-only so-long-variable-overrides 'assq)
  ;; ...but at least reduce the level of syntax highlighting
  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))
  ;; ...and insist that save-place not operate in large/long files
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))

  ;; But disable everything else that may be unnecessary/expensive for large or wide buffers.
  (appendq! so-long-minor-modes
            '(spell-fu-mode
              eldoc-mode
              highlight-numbers-mode
              better-jumper-local-mode
              ws-butler-mode
              auto-composition-mode
              undo-tree-mode
              highlight-indent-guides-mode
              hl-fill-column-mode
              ;; These are redundant on Emacs 29+
              flycheck-mode
              smartparens-mode
              smartparens-strict-mode))
  )

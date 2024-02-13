;;; doom-ui.el --- defaults for Doom's aesthetics -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Variables

(defcustom doom-theme nil
  "A symbol representing the Emacs theme to load at startup.

Set to `nil' to load no theme at all. This variable is changed by
`load-theme'.")


(defcustom doom-font nil
  "The default font to use.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string.

This affects the `default' and `fixed-pitch' faces.

Examples:
  (setq doom-font (font-spec :family \"Fira Mono\" :size 12))
  (setq doom-font \"Terminus (TTF):pixelsize=12:antialias=off\")
  (setq doom-font \"Fira Code-14\")")


(defcustom doom-variable-pitch-font nil
  "The default font to use for variable-pitch text.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string. See
`doom-font' for examples.

An omitted font size means to inherit `doom-font''s size.")


(defcustom doom-serif-font nil
  "The default font to use for the `fixed-pitch-serif' face.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string. See
`doom-font' for examples.

An omitted font size means to inherit `doom-font''s size.")


(defcustom doom-symbol-font nil
  "Fallback font for symbols.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string. See
`doom-font' for examples. Emacs defaults to Symbola.

WARNING: if you specify a size for this font it will hard-lock any usage of this
font to that size. It's rarely a good idea to do so!")


(define-obsolete-variable-alias 'doom-unicode-font 'doom-symbol-font "3.0.0")


(defcustom doom-emoji-font nil
  "Fallback font for emoji.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string. See
`doom-font' for examples.

WARNING: if you specify a size for this font it will hard-lock any usage of this
font to that size. It's rarely a good idea to do so!")


(defconst doom-emoji-fallback-font-families
  '("Apple Color Emoji"
    "Segoe UI Emoji"
    "Noto Color Emoji"
    "Noto Emoji")
  "A list of fallback font families to use for emojis.
These are platform-specific fallbacks for internal use. If you
want to change your emoji font, use `doom-emoji-font'.")


(defconst doom-symbol-fallback-font-families
  '("Segoe UI Symbol"
    "Apple Symbols")
  "A list of fallback font families for general symbol glyphs.
These are platform-specific fallbacks for internal use. If you
want to change your symbol font, use `doom-symbol-font'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom hooks

(defcustom doom-init-ui-hook nil
  "List of hooks to run when the UI has been initialized.")

(defcustom doom-load-theme-hook nil
  "Hook run after the theme is loaded with `load-theme' or reloaded with
`doom/reload-theme'.")

(defcustom doom-switch-buffer-hook nil
  "A list of hooks run after changing the current buffer.")

(defcustom doom-switch-window-hook nil
  "A list of hooks run after changing the focused windows.")

(defcustom doom-switch-frame-hook nil
  "A list of hooks run after changing the focused frame.")


(defun doom-run-switch-buffer-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (run-hooks 'doom-switch-buffer-hook)))


(defvar doom--last-frame nil)
(defun doom-run-switch-window-or-frame-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (unless (equal (old-selected-frame) (selected-frame))
      (run-hooks 'doom-switch-frame-hook))
    (unless (or (minibufferp)
                (equal (old-selected-window) (minibuffer-window)))
      (run-hooks 'doom-switch-window-hook))))


(defun doom-protect-fallback-buffer-h ()
  "Don't kill the scratch buffer. Meant for `kill-buffer-query-functions'."
  (not (eq (current-buffer) (doom-fallback-buffer))))


(defun doom-highlight-non-default-indentation-h ()
  "Highlight whitespace at odds with `indent-tabs-mode'.
That is, highlight tabs if `indent-tabs-mode' is `nil', and highlight spaces at
the beginnings of lines if `indent-tabs-mode' is `t'. The purpose is to make
incorrect indentation in the current buffer obvious to you.

Does nothing if `whitespace-mode' or `global-whitespace-mode' is already active
or if the current buffer is read-only or not file-visiting."
  (unless (or (eq major-mode 'fundamental-mode)
              (bound-and-true-p global-whitespace-mode)
              (null buffer-file-name))
    (require 'whitespace)
    (set (make-local-variable 'whitespace-style)
         (cl-union (if indent-tabs-mode
                       '(indentation)
                     '(tabs tab-mark))
                   (when whitespace-mode
                     (remq 'face whitespace-active-style))))
    (cl-pushnew 'face whitespace-style) ; must be first
    (whitespace-mode +1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Theme & font
;;

(defun doom--make-font-specs (face font)
  ;; font is unused
  ;; (message ">>> Run doom--make-font-specs %s %s" face font)
  (let* ((base-specs (cadr (assq 'user (get face 'theme-face))))
         (base-specs (or base-specs '((t nil))))
         (attrs '(:family :foundry :slant :weight :height :width))
         (new-specs nil))
    (dolist (spec base-specs)
      ;; Each SPEC has the form (DISPLAY ATTRIBUTE-PLIST)
      (let ((display (car spec))
            (plist   (copy-tree (nth 1 spec))))
        ;; Alter only DISPLAY conditions matching this frame.
        (when (or (memq display '(t default))
                  (face-spec-set-match-display display this-frame))
          (dolist (attr attrs)
            (setq plist (plist-put plist attr (face-attribute face attr)))))
        (push (list display plist) new-specs)))
    ;; (message ">>> %s" new-specs)
    (nreverse new-specs)))


(defun doom-init-fonts-h (&optional reload)
  "Loads `doom-font'."
  (message ">>> Run doom-init-fonts-h %s" doom-font)
  (dolist (map `((default . ,doom-font)
                 (fixed-pitch . ,doom-font)
                 (fixed-pitch-serif . ,doom-serif-font)
                 (variable-pitch . ,doom-variable-pitch-font)))
    (when-let* ((face (car map))
                (font (cdr map)))
      (dolist (frame (frame-list))
        (when (display-multi-font-p frame)
          ;; set the font
          (set-face-attribute face frame
                              :width 'normal :weight 'normal
                              :slant 'normal :font font)))
      (let ((new-specs (doom--make-font-specs face font)))
        ;; Don't save to `customized-face' so it's omitted from `custom-file'
        ;; (put face 'customized-face new-specs)
        (custom-push-theme 'theme-face face 'user 'set new-specs)
        (put face 'face-modified nil))))
  (when (fboundp 'set-fontset-font)
    (let* ((fn (doom-rpartial #'member (font-family-list)))
           (symbol-font (or doom-symbol-font
                            (cl-find-if fn doom-symbol-fallback-font-families)))
           (emoji-font (or doom-emoji-font
                           (cl-find-if fn doom-emoji-fallback-font-families))))
      (when symbol-font
        (dolist (script '(symbol mathematical))
          (set-fontset-font t script symbol-font)))
      (when emoji-font
        ;; DEPRECATED: make unconditional when we drop 27 support
        (when (version<= "28.1" emacs-version)
          (set-fontset-font t 'emoji emoji-font))
        ;; some characters in the Emacs symbol script are often covered by emoji
        ;; fonts
        (set-fontset-font t 'symbol emoji-font nil 'append)))
    ;; Nerd Fonts use these Private Use Areas
    (dolist (range '((#xe000 . #xf8ff) (#xf0000 . #xfffff)))
      (set-fontset-font t range "Symbols Nerd Font Mono")))
  ;; Users should inject their own font logic in `after-setting-font-hook'
  (run-hooks 'after-setting-font-hook))


(defun doom-init-theme-h (&rest _)
  "Load the theme specified by `doom-theme' in FRAME."
  (message ">>> Run doom-init-theme-h %s" doom-theme)
  (when (and doom-theme (not (custom-theme-enabled-p doom-theme)))
    (load-theme doom-theme t)))


(defadvice! doom--load-theme-a (fn theme &optional no-confirm no-enable)
  "Record `doom-theme', disable old themes, and trigger `doom-load-theme-hook'."
  :around #'load-theme
  ;; Run `load-theme' from an estranged buffer, where we can ensure that
  ;; buffer-local face remaps (by `mixed-pitch-mode', for instance) won't
  ;; interfere with recalculating faces in new themes.
  (with-temp-buffer
    (let ((last-themes (copy-sequence custom-enabled-themes)))
      ;; Disable previous themes so there are no conflicts. If you truly want
      ;; multiple themes enabled, then use `enable-theme' instead.
      (mapc #'disable-theme custom-enabled-themes)
      (prog1 (funcall fn theme no-confirm no-enable)
        (when (and (not no-enable) (custom-theme-enabled-p theme))
          (setq doom-theme theme)
          (put 'doom-theme 'previous-themes (or last-themes 'none))
          ;; DEPRECATED Hook into `enable-theme-functions' when we target 29
          (doom-run-hooks 'doom-load-theme-hook))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Bootstrap

(defun doom-init-ui-h (&optional _)
  "Initialize Doom's user interface by applying all its advice and hooks.

These should be done as late as possible, as to avoid/minimize prematurely
triggering hooks during startup."
  (message ">>> Run doom-init-ui-h")
  (doom-run-hooks 'doom-init-ui-hook)

  (add-hook 'kill-buffer-query-functions #'doom-protect-fallback-buffer-h)
  (add-hook 'after-change-major-mode-hook #'doom-highlight-non-default-indentation-h 'append)

  ;; Make `next-buffer', `other-buffer', etc. ignore unreal buffers.
  (push '(buffer-predicate . doom-buffer-frame-predicate) default-frame-alist)

  ;; Initialize `doom-switch-window-hook' and `doom-switch-frame-hook'
  (add-hook 'window-selection-change-functions #'doom-run-switch-window-or-frame-hooks-h)
  ;; Initialize `doom-switch-buffer-hook'
  (add-hook 'window-buffer-change-functions #'doom-run-switch-buffer-hooks-h)
  ;; `window-buffer-change-functions' doesn't trigger for files visited via the server.
  (add-hook 'server-visit-hook #'doom-run-switch-buffer-hooks-h))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'doom-ui)
;;; doom-ui.el ends here

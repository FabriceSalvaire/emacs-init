See also `__doom_sync_notes__.md`

# Content

This directory contains lisp code from Doom Emacs:

``` sh
ag --nofilename '^\(' | sed -e 's/ .*//' | sort | uniq

ag --nonumbers '^\(defun'
```

```
(defadvice!
(defalias
(defbackport!
(defconst
(defcustom
(defgroup
(define-error
(define-minor-mode
(define-obsolete-function-alias
(define-obsolete-variable-alias
(defmacro
(defun
(defvar
(defvar-local

(setplist
```

# defun

```
buffers.el
(defun doom-buffer-frame-predicate (buf)
(defun doom-fallback-buffer ()
(defun doom-project-buffer-list (&optional project)
(defun doom-open-projects ()
(defun doom-dired-buffer-p (buf)
(defun doom-special-buffer-p (buf)
(defun doom-temp-buffer-p (buf)
(defun doom-visible-buffer-p (buf)
(defun doom-buried-buffer-p (buf)
(defun doom-non-file-visiting-buffer-p (buf)
(defun doom-real-buffer-list (&optional buffer-list)
(defun doom-real-buffer-p (buffer-or-name)
(defun doom-unreal-buffer-p (buffer-or-name)
(defun doom-buffers-in-mode (modes &optional buffer-list derived-p)
(defun doom-visible-windows (&optional window-list)
(defun doom-visible-buffers (&optional buffer-list)
(defun doom-buried-buffers (&optional buffer-list)
(defun doom-matching-buffers (pattern &optional buffer-list)
(defun doom-set-buffer-real (buffer flag)
(defun doom-kill-buffer-and-windows (buffer)
(defun doom-fixup-windows (windows)
(defun doom-kill-buffer-fixup-windows (buffer)
(defun doom-kill-buffers-fixup-windows (buffers)
(defun doom-kill-matching-buffers (pattern &optional buffer-list)
(defun doom-mark-buffer-as-real-h ()
(defun doom/save-and-kill-buffer ()
(defun doom/kill-this-buffer-in-all-windows (buffer &optional dont-save)
(defun doom--message-or-count (interactive message count)
(defun doom/kill-all-buffers (&optional buffer-list interactive)
(defun doom/kill-other-buffers (&optional buffer-list interactive)
(defun doom/kill-matching-buffers (pattern &optional buffer-list interactive)
(defun doom/kill-buried-buffers (&optional buffer-list interactive)
(defun doom/kill-project-buffers (project &optional interactive)

doom-lib.el
(defun doom--log (text &rest args)
(defun doom--resolve-hook-forms (hooks)
(defun doom--setq-hook-fns (hooks rest &optional singles)
(defun doom-unquote (exp)
(defun doom-keyword-intern (str)
(defun doom-keyword-name (keyword)
(defun doom-rpartial (fn &rest args)
(defun doom-lookup-key (keys &rest keymaps)
(defun doom-load (path &optional noerror)
(defun doom-require (feature &optional filename noerror)
(defun doom-load-envvars-file (file &optional noerror)
(defun doom-run-hook (hook)
(defun doom-run-hooks (&rest hooks)
(defun doom-run-hook-on (hook-var trigger-hooks)
(defun doom-compile-functions (&rest fns)
(defun doom--fn-crawl (data args)

doom-themes.el
(defun doom-themes--colors-p (item)
(defun doom-themes--apply-faces (new-faces &optional default-faces)
(defun doom-themes--colorize (item type)
(defun doom-themes--build-face (face)
(defun doom-name-to-rgb (color)
(defun doom-blend (color1 color2 alpha)
(defun doom-darken (color alpha)
(defun doom-lighten (color alpha)
(defun doom-color (name &optional type)
(defun doom-ref (face prop &optional class)
(defun doom-themes-prepare-facelist (custom-faces)
(defun doom-themes-prepare-varlist (vars)
(defun doom-themes-set-faces (theme &rest faces)

doom-ui.el
(defun doom-run-switch-buffer-hooks-h (&optional _)
(defun doom-run-switch-window-or-frame-hooks-h (&optional _)
(defun doom-protect-fallback-buffer-h ()
(defun doom-highlight-non-default-indentation-h ()
(defun doom--make-font-specs (face font)
(defun doom-init-fonts-h (&optional reload)
(defun doom-init-theme-h (&rest _)
(defun doom-init-ui-h (&optional _)

fonts.el
(defun doom-normalize-font (font)
(defun doom-adjust-font-size (increment &optional fixed-size-p font-alist)
(defun doom-font-exists-p (font)
(defun doom/reload-font ()
(defun doom/increase-font-size (count &optional increment)
(defun doom/decrease-font-size (count &optional increment)
(defun doom/reset-font-size ()

plist.el
(defun doom-plist-get (plist prop &optional nil-value)
(defun doom-plist-merge (from-plist to-plist)
(defun doom-plist-delete-nil (plist)
(defun doom-plist-keys (plist)
(defun doom-plist-values (plist)

ui.el
(defun doom-resize-window (window new-size &optional horizontal force-p)
(defun doom-quit-p (&optional prompt)
(defun doom-recenter-a (&rest _)
(defun doom-preserve-window-position-a (fn &rest args)
(defun doom-shut-up-a (fn &rest args)
(defun doom-apply-ansi-color-to-compilation-buffer-h ()
(defun doom-disable-show-paren-mode-h ()
(defun doom-enable-line-numbers-h ()
(defun doom-disable-line-numbers-h ()
(defun doom/toggle-line-numbers ()
(defun doom/delete-frame-with-prompt ()
(defun doom--enlargened-forget-last-wconf-h ()
(defun doom/window-maximize-buffer (&optional arg)
(defun doom/window-enlargen (&optional arg)
(defun doom/window-maximize-horizontally ()
(defun doom/window-maximize-vertically ()
(defun doom/set-frame-opacity (opacity)
(defun doom/narrow-buffer-indirectly (beg end)
(defun doom/widen-indirectly-narrowed-buffer (&optional arg)
(defun doom/toggle-narrow-buffer (beg end)
```

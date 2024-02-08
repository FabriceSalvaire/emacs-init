;;; core.el --- Core Settings -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Core Settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See Doom doom-ui.el
;;          doom-editor.py

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Comfirmation

;; A simple confirmation prompt when killing Emacs. But only prompt when there
;; are real buffers open.
;; Fixme: works ???
(setq confirm-kill-emacs #'doom-quit-p)
;; Prompt for confirmation when deleting a non-empty frame; a last line of
;; defense against accidental loss of work.
(global-set-key [remap delete-frame] #'doom/delete-frame-with-prompt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Recent — minor mode that builds a list of recently opened files
;;   https://www.emacswiki.org/emacs/RecentFiles

;; Enable menu for recently opened files
(recentf-mode t)
;; (setq recentf-max-menu-items 25)
;; (setq recentf-max-saved-items 25)
;; Periodically saving the list of files
;; (run-at-time nil (* 5 60) 'recentf-save-list)
;; (global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Buffer Name

;; Uniquify — overrides Emacs default mechanism for making buffer names unique
;;  https://www.emacswiki.org/emacs/uniquify
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t ; rename after killing uniquified
      uniquify-ignore-buffers-re "^\\*" ; don't muck with special buffers (or Gnus mail buffers)
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mouse

;; middle-click paste at point, not at click
(setq mouse-yank-at-point t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Scrolling

(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the cursor more than N
      ;; lines past window edges (where N is the settings of `scroll-conservatively'). This is
      ;; especially slow in larger files during large-scale scrolling commands. If kept over 100,
      ;; the window is never automatically recentered. The default (0) triggers this too
      ;; aggressively, so I've set it to 10 to recenter if scrolling too far off-screen.
      scroll-conservatively 10
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll' for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Cursor

;; The blinking cursor is distracting, but also interferes with cursor settings in some minor modes
;; that try to change it buffer-locally (like treemacs) and can cause freezing for folks (esp on
;; macOS) with customized & color cursors.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting, especially for tabs.
(setq x-stretch-cursor nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Buffers
;;

;; (defadvice! doom--switch-to-fallback-buffer-maybe-a (&rest _)
;;   "Switch to `doom-fallback-buffer' if on last real buffer.

;; Advice for `kill-current-buffer'. If in a dedicated window, delete it. If there
;; are no real buffers left OR if all remaining buffers are visible in other
;; windows, switch to `doom-fallback-buffer'. Otherwise, delegate to original
;; `kill-current-buffer'."
;;   :before-until #'kill-current-buffer
;;   (let ((buf (current-buffer)))
;;     (cond ((window-dedicated-p)
;;            (delete-window)
;;            t)
;;           ((eq buf (doom-fallback-buffer))
;;            (message "Can't kill the fallback buffer.")
;;            t)
;;           ((doom-real-buffer-p buf)
;;            (let ((visible-p (delq (selected-window) (get-buffer-window-list buf nil t))))
;;              (unless visible-p
;;                (when (and (buffer-modified-p buf)
;;                           (not (y-or-n-p
;;                                 (format "Buffer %s is modified; kill anyway?"
;;                                         buf))))
;;                  (user-error "Aborted")))
;;              (let ((inhibit-redisplay t)
;;                    buffer-list-update-hook)
;;                (when (or ;; if there aren't more real buffers than visible buffers,
;;                       ;; then there are no real, non-visible buffers left.
;;                       (not (cl-set-difference (doom-real-buffer-list)
;;                                               (doom-visible-buffers)))
;;                       ;; if we end up back where we start (or previous-buffer
;;                       ;; returns nil), we have nowhere left to go
;;                       (memq (switch-to-prev-buffer nil t) (list buf 'nil)))
;;                  (switch-to-buffer (doom-fallback-buffer)))
;;                (unless visible-p
;;                  (with-current-buffer buf
;;                    (restore-buffer-modified-p nil))
;;                  (kill-buffer buf)))
;;              (run-hooks 'buffer-list-update-hook)
;;              t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Fringes

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like git-gutter and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Windows/frames

;; A simple frame title
;; (setq frame-title-format '("%b – Doom Emacs")
;;       icon-title-format frame-title-format)

;; Don't resize the frames in steps; it looks weird, especially in tiling window managers, where it
;; can leave unseemly gaps.
(setq frame-resize-pixelwise t)

;; But do not resize windows pixelwise, this can cause crashes in some cases when resizing too many
;; windows at once or rapidly.
(setq window-resize-pixelwise nil)

;; UX: GUIs are inconsistent across systems, desktop environments, and themes, and don't match the
;;   look of Emacs. They also impose inconsistent shortcut key paradigms. I'd rather Emacs be
;;   responsible for prompting.
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;; FIX: The native border "consumes" a pixel of the fringe on righter-most splits, `window-divider'
;;   does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'doom-init-ui-hook #'window-divider-mode)

;; UX: Favor vertical splits over horizontal ones. Monitors are trending toward wide, rather than
;;   tall.
(setq split-width-threshold 160
      split-height-threshold nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Minibuffer

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command while we're in the
;; minibuffer.
(setq enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any feedback after typing is
;; better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only)

;; Typing yes/no is obnoxious when y/n will do
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  ;; DEPRECATED: Remove when we drop 27.x support
  ;; (advice-add #'yes-or-no-p :override #'y-or-n-p)
  )

;; Try to keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Optimise for large file
;; 

(defvar doom-inhibit-large-file-detection nil
  "If non-nil, inhibit large/long file detection when opening files.")

(defvar doom-large-file-p nil)
(put 'doom-large-file-p 'permanent-local t)

(defvar doom-large-file-size-alist '(("." . 1.0))
  "An alist mapping regexps (like `auto-mode-alist') to filesize thresholds.

If a file is opened and discovered to be larger than the threshold, Doom
performs emergency optimizations to prevent Emacs from hanging, crashing or
becoming unusably slow.

These thresholds are in MB, and is used by `doom--optimize-for-large-files-a'.")

(defvar doom-large-file-excluded-modes
  '(so-long-mode special-mode archive-mode tar-mode jka-compr
    git-commit-mode image-mode doc-view-mode doc-view-mode-maybe
    ebrowse-tree-mode pdf-view-mode tags-table-mode)
  "Major modes that `doom-check-large-file-h' will ignore.")


(defadvice! doom--prepare-for-large-files-a (size _ filename &rest _)
  "Sets `doom-large-file-p' if the file is considered large.

Uses `doom-large-file-size-alist' to determine when a file is too large. When
`doom-large-file-p' is set, other plugins can detect this and reduce their
runtime costs (or disable themselves) to ensure the buffer is as fast as
possible."
  :before #'abort-if-file-too-large
  (and (numberp size)
       (null doom-inhibit-large-file-detection)
       (ignore-errors
         (> size
            (* 1024 1024
               (assoc-default filename doom-large-file-size-alist
                              #'string-match-p))))
       (setq-local doom-large-file-p size)))

(add-hook! 'find-file-hook
  (defun doom-optimize-for-large-files-h ()
    "Trigger `so-long-minor-mode' if the file is large."
    (when (and doom-large-file-p buffer-file-name)
      (if (or doom-inhibit-large-file-detection
              (memq major-mode doom-large-file-excluded-modes))
          (kill-local-variable 'doom-large-file-p)
        (when (fboundp 'so-long-minor-mode) ; in case the user disabled it
          (so-long-minor-mode +1))
        (message "Large file detected! Cutting a few corners to improve performance...")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Symlinks / Missing directory

;; Resolve symlinks when opening files, so that any operations are conducted
;; from the file's true directory (like `find-file').
;; (setq find-file-visit-truename t
;;       vc-follow-symlinks t)

;; Disable the warning "X and Y are the same file". It's fine to ignore this
;; warning as it will redirect you to the existing buffer anyway.
;; (setq find-file-suppress-same-file-warnings t)

;; Create missing directories when we open a file that doesn't exist under a
;; directory tree that may not exist.
(add-hook! 'find-file-not-found-functions
  (defun doom-create-missing-directories-h ()
    "Automatically create missing directories when creating new files."
    (unless (file-remote-p buffer-file-name)
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (and (not (file-directory-p parent-directory))
             (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                               parent-directory))
             (progn (make-directory parent-directory 'parents)
                    t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Backup / Lockfile / Autosave
;;
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Save-Files.html

;; Don't generate backups or lockfiles. While auto-save maintains a copy so long as a buffer is
;; unsaved, backups create copies once, when the file is first written, and never again until it is
;; killed and reopened. This is better suited to version control, and I don't want world-readable
;; copies of potentially sensitive material floating around our filesystem.
(setq create-lockfiles nil
      make-backup-files nil

      ;; But in case the user does enable it, some sensible defaults:
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      ;; Fixme: for filename clash read doc...
      ;; backup-directory-alist (list (cons "." (concat doom-cache-dir "backup/")))
      ;; tramp-backup-directory-alist backup-directory-alist
      )


;; But turn on auto-save, so we have a fallback in case of crashes or lost data.
;; Use `recover-file' or `recover-session' to recover them.
(setq auto-save-default t
      ;; Don't auto-disable auto-save after deleting big chunks. This defeats
      ;; the purpose of a failsafe. This adds the risk of losing the data we
      ;; just deleted, but I believe that's VCS's jurisdiction, not ours.
      auto-save-include-big-deletions t

      ;; Keep it out of `doom-emacs-dir' or the local directory.
      ;;   "$HOME/.config/emacs-legacy/auto-save-list/.saves-"
      ;; auto-save-list-file-prefix (concat doom-cache-dir "autosave/")
      ;; tramp-auto-save-directory  (concat doom-cache-dir "tramp-autosave/")

      ;; auto-save-file-name-transforms
      ;; (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
      ;;             ;; Prefix tramp autosaves to prevent conflicts with local ones
      ;;             (concat auto-save-list-file-prefix "tramp-\\2") t)
      ;;       (list ".*" auto-save-list-file-prefix t))
      )

;; (defadvice! doom--shut-up-autosave-a (fn &rest args)
;;   "If a file has autosaved data, `after-find-file' will pause for 1 second to
;; tell you about it. Very annoying. This prevents that."
;;   :around #'after-find-file
;;   (letf! ((#'sit-for #'ignore))
;;     (apply fn args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Guess Mode for New File

(add-hook! 'after-save-hook
  (defun doom-guess-mode-h ()
    "Guess major mode when saving a file in `fundamental-mode'.

Likely, something has changed since the buffer was opened. e.g. A shebang line
or file path may exist now."
    (when (eq major-mode 'fundamental-mode)
      (let ((buffer (or (buffer-base-buffer) (current-buffer))))
        (and (buffer-file-name buffer)
             (eq buffer (window-buffer (selected-window))) ; only visible buffers
             (set-auto-mode)
             (not (eq major-mode 'fundamental-mode)))))))

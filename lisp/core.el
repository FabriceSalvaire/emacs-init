;;; core.el --- Core Settings -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; loaded after ui.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Session...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; Auto Revert
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Revert.html

(use-package autorevert
  ;; revert buffers when their files/state have changed
  :hook (focus-in . doom-auto-revert-buffers-h)
  :hook (after-save . doom-auto-revert-buffers-h)
  ;; Fixme: doom-switch-...-hook
  ;; :hook (doom-switch-buffer . doom-auto-revert-buffer-h)
  ;; :hook (doom-switch-window . doom-auto-revert-buffer-h)
  :config
  (setq auto-revert-verbose t ; let us know when it happens
        auto-revert-use-notify nil
        auto-revert-stop-on-user-input nil
        ;; Only prompts for confirmation when buffer is unsaved.
        revert-without-query (list "."))

  ;; `auto-revert-mode' and `global-auto-revert-mode' would, normally, abuse the
  ;; heck out of file watchers _or_ aggressively poll your buffer list every X
  ;; seconds. Too many watchers can grind Emacs to a halt if you preform
  ;; expensive or batch processes on files outside of Emacs (e.g. their mtime
  ;; changes), and polling your buffer list is terribly inefficient as your
  ;; buffer list grows into the hundreds.
  ;;
  ;; Doom does this lazily instead. i.e. All visible buffers are reverted
  ;; immediately when a) a file is saved or b) Emacs is refocused (after using
  ;; another app). Meanwhile, buried buffers are reverted only when they are
  ;; switched to. This way, Emacs only ever has to operate on, at minimum, a
  ;; single buffer and, at maximum, ~10 buffers (after all, when do you ever
  ;; have more than 10 windows in any single frame?).
  (defun doom-auto-revert-buffer-h ()
    "Auto revert current buffer, if necessary."
    (unless (or auto-revert-mode (active-minibuffer-window))
      (let ((auto-revert-mode t))
        (auto-revert-handler))))

  (defun doom-auto-revert-buffers-h ()
    "Auto revert stale buffers in visible windows, if necessary."
    (dolist (buf (doom-visible-buffers))
      (with-current-buffer buf
        (doom-auto-revert-buffer-h)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; savehist
;;   https://www.emacswiki.org/emacs/SaveHist

(use-package savehist
  ;; persist variables across sessions
  ;;D;; :defer-incrementally custom
  :hook (doom-first-input . savehist-mode)

  ;; :custom
  ;;D;; (savehist-file (concat doom-cache-dir "savehist"))
  :config
  (setq savehist-save-minibuffer-history t
        savehist-autosave-interval nil     ; save on kill only
        savehist-additional-variables
        '(kill-ring                        ; persist clipboard
          register-alist                   ; persist macros
          mark-ring global-mark-ring       ; persist marks
          search-ring regexp-search-ring)) ; persist searches

  (add-hook! 'savehist-save-hook
    (defun doom-savehist-unpropertize-variables-h ()
      "Remove text properties from `kill-ring' to reduce savehist cache size."
      (setq kill-ring
            (mapcar #'substring-no-properties
                    (cl-remove-if-not #'stringp kill-ring))
            register-alist
            (cl-loop for (reg . item) in register-alist
                     if (stringp item)
                     collect (cons reg (substring-no-properties item))
                     else collect (cons reg item))))

    (defun doom-savehist-remove-unprintable-registers-h ()
      "Remove unwriteable registers (e.g. containing window configurations).
Otherwise, `savehist' would discard `register-alist' entirely if we don't omit
the unwritable tidbits."
      ;; Save new value in the temp buffer savehist is running
      ;; `savehist-save-hook' in. We don't want to actually remove the
      ;; unserializable registers in the current session!
      (setq-local register-alist
                  (cl-remove-if-not #'savehist-printable register-alist))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; saveplace — persistent point location in buffers
;;   https://www.emacswiki.org/emacs/SaveHist

(use-package saveplace
  :hook (doom-first-file . save-place-mode)

  ;; :custom
  ;;D;; (save-place-file (concat doom-cache-dir "saveplace"))
  :config
  (defadvice! doom--recenter-on-load-saveplace-a (&rest _)
    "Recenter on cursor when loading a saved place."
    :after-while #'save-place-find-file-hook
    (if buffer-file-name (ignore-errors (recenter))))

  (defadvice! doom--inhibit-saveplace-in-long-files-a (fn &rest args)
    :around #'save-place-to-alist
    (unless doom-large-file-p
      (apply fn args)))

  (defadvice! doom--dont-prettify-saveplace-cache-a (fn)
    "`save-place-alist-to-file' uses `pp' to prettify the contents of its cache.
`pp' can be expensive for longer lists, and there's no reason to prettify cache
files, so this replace calls to `pp' with the much faster `prin1'."
    :around #'save-place-alist-to-file
    (letf! ((#'pp #'prin1)) (funcall fn)))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helpfull
;;   https://github.com/Wilfred/helpful

(use-package helpful
  :commands helpful--read-symbol
  :hook (helpful-mode . visual-line-mode)

  :init
  ;; Make `apropos' et co search more extensively. They're more useful this way.
  (setq apropos-do-all t)

  ;; Fixme: don't work
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-function] #'helpful-callable) ; C-h f / F1 f
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol)
  (global-set-key [remap describe-variable] #'helpful-variable)

  (defun doom-use-helpful-a (fn &rest args)
    "Force FN to use helpful instead of the old describe-* commands."
    (letf! ((#'describe-function #'helpful-function)
            (#'describe-variable #'helpful-variable))
      (apply fn args)))

  (after! apropos
    ;; patch apropos buttons to call helpful instead of help
    (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
      (button-type-put
       fun-bt 'action
       (lambda (button)
         (helpful-callable (button-get button 'apropos-symbol)))))
    (dolist (var-bt '(apropos-variable apropos-user-option))
      (button-type-put
       var-bt 'action
       (lambda (button)
         (helpful-variable (button-get button 'apropos-symbol))))))

  (when (> emacs-major-version 28)
    ;; REVIEW This should be reported upstream to Emacs.
    (defadvice! doom--find-function-search-for-symbol-save-excursion-a (fn &rest args)
      "Suppress cursor movement by `find-function-search-for-symbol'.

Addresses an unwanted side-effect in `find-function-search-for-symbol' on Emacs
29 where the cursor is moved to a variable's definition if it's defined in the
current buffer."
      :around #'find-function-search-for-symbol
      (let (buf pos)
        (letf! (defun find-library-name (library)
                 (let ((filename (funcall find-library-name library)))
                   (with-current-buffer (find-file-noselect filename)
                     (setq buf (current-buffer)
                           pos (point)))
                   filename))
          (prog1 (apply fn args)
            (when (buffer-live-p buf)
              (with-current-buffer buf (goto-char pos))))))))

  :config
  (setq helpful-set-variable-function #'setq!)
  )

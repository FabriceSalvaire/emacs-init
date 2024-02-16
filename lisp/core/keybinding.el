;;; keybinding.el --- keybinding -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Fixme: clean evil...
;;
;; A centralized keybinds system, integrated with `which-key' to preview available keybindings.
;; All built into one powerful macro: `map!'.
;;
;; See doom-lib/keys.el for library code.
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HACK: Emacs cannot distinguish between C-i from TAB. This is largely a byproduct of its history
;; in the terminal, which can't distinguish them either, however, when GUIs came about Emacs greated
;; separate input events for more contentious keys like TAB and RET.
;; Therefore [return] != RET, [tab] != TAB, and [backspace] != DEL.
;;
;; In the same vein, this keybind adds a [C-i] event, so users can bind to it.
;; Otherwise, it falls back to regular C-i keybinds.
(define-key key-translation-map [?\C-i]
  (cmd! (if (let ((keys (this-single-command-raw-keys)))
              (and keys
                   (not (cl-position 'tab    keys))
                   (not (cl-position 'kp-tab keys))
                   (display-graphic-p)
                   ;; Fall back if no <C-i> keybind can be found, otherwise
                   ;; we've broken all pre-existing C-i keybinds.
                   (let ((key
                          (doom-lookup-key
                           (vconcat (cl-subseq keys 0 -1) [C-i]))))
                     (not (or (numberp key) (null key))))))
            [C-i] [?\C-i])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Universal, non-nuclear escape

;; `keyboard-quit' is too much of a nuclear option. I wanted an ESC/C-g to do-what-I-mean.
;;  It serves four purposes (in order):
;;
;; 1. Quit active states; e.g. highlights, searches, snippets, iedit,
;;    multiple-cursors, recording macros, etc.
;; 2. Close popup windows remotely (if it is allowed to)
;; 3. Refresh buffer indicators, like git-gutter and flycheck
;; 4. Or fall back to `keyboard-quit'
;;
;; And it should do these things incrementally, rather than all at once. And it shouldn't interfere
;; with recording macros or the minibuffer. This may require you press ESC/C-g two or three times on
;; some occasions to reach `keyboard-quit', but this is much more intuitive.

(defvar doom-escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users).

More specifically, when `doom/escape' is pressed. If any hook returns non-nil,
all hooks after it are ignored.")

(defun doom/escape (&optional interactive)
  "Run `doom-escape-hook'."
  (interactive (list 'interactive))
  (let ((inhibit-quit t))
    (cond ((minibuffer-window-active-p (minibuffer-window))
           ;; quit the minibuffer if open.
           (when interactive
             (setq this-command 'abort-recursive-edit))
           (abort-recursive-edit))
          ;; Run all escape hooks. If any returns non-nil, then stop there.
          ((run-hook-with-args-until-success 'doom-escape-hook))
          ;; don't abort macros
          ((or defining-kbd-macro executing-kbd-macro) nil)
          ;; Back to the default
          ((unwind-protect (keyboard-quit)
             (when interactive
               (setq this-command 'keyboard-quit)))))))

(global-set-key [remap keyboard-quit] #'doom/escape)

(with-eval-after-load 'eldoc
  (eldoc-add-command 'doom/escape))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; which-key — displays available keybindings in popup
;;  htpps://github.com/justbur/emacs-which-key
;;  which-key is a minor mode for Emacs that displays the key bindings following your currently
;;  entered incomplete command (a prefix) in a popup.
;;
;;  paging navigation: C-h n

(use-package which-key
  ;;s;; :ensure t
  :hook (doom-first-input . which-key-mode)

  :init
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)

  :config
  ;; https://github.com/justbur/emacs-which-key#key-and-description-replacement
  ;; (put 'which-key-replacement-alist 'initial-value which-key-replacement-alist)
  ;; (add-hook! 'doom-before-reload-hook
  ;;   (defun doom-reset-which-key-replacements-h ()
  ;;     (setq which-key-replacement-alist (get 'which-key-replacement-alist 'initial-value))))
  ;; general improvements to which-key readability
  (which-key-setup-side-window-bottom)
  ;; (which-key-setup-side-window-right)
  ;; (which-key-setup-side-window-right-bottom)
  (setq-hook! 'which-key-init-buffer-hook line-spacing 3)

  ;; (which-key-add-key-based-replacements doom-leader-key "<leader>")
  ;; (which-key-add-key-based-replacements doom-localleader-key "<localleader>")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'keybinding)
;;; keybinding.el ends here

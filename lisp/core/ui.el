;;; ui.el --- UI Settings -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; UI Settings
;;   loaded after startup.el
;;   - fullscreen window
;;   - enable/disable ...bar
;;   - theme
;;   - icons
;;   - mouse
;;   - cursor
;;   - windows/frame
;;   - scrolling
;;   - horizontal line
;;   - fringes
;;   - minibuffer
;;   - modeline
;;   - line / column mode
;;
;; See Doom doom-ui.el
;;          doom-editor.py
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set Fullscreen

(pcase window-system
  ('w32 (set-frame-parameter nil 'fullscreen 'fullboth))
  (_ (set-frame-parameter nil 'fullscreen 'maximized)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ...Bar

;; Disable the tool bar
(tool-bar-mode -1)

;; Speedbar : buffer to navigate
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Speedbar.html
;;   https://www.gnu.org/software/emacs/manual/html_node/speedbar/index.html#Top
;;   M-x speedbar
;; (speedbar t)

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
;; Theme -> ./themes
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Creating-Custom-Themes.html#Creating-Custom-Themes
;;   M-x list-faces-display
;;   M-x customize-create-theme
;;   M-x describe-face

;; (load-theme 'fabrice-dark t)

(setq doom-font (font-spec
                 :foundry "ADBO"
                 :family "Source Code Pro"
                 :size 16 ; => :height 120
                 :weight 'medium
                 :slant 'normal
                 :width 'normal
                 ))
;; (custom-set-faces '(default ((t (:family "Source Code Pro"
;;                                  :foundry "ADBO"
;;                                  :weight medium
;;                                  :slant normal
;;                                  :width normal
;;                                  :height 120)))))
;; (setq doom-theme 'doom-one)
(setq doom-theme 'fabrice-dark)

;; Apply fonts and theme
(let ((hook (if (daemonp)
                'server-after-make-frame-hook
              'after-init-hook)))
  (add-hook hook #'doom-init-fonts-h -100)
  (add-hook hook #'doom-init-theme-h -90))

;; PERF: Init UI late, but not too late. Its impact on startup time seems to
;;   vary wildly depending on exact placement. `window-setup-hook' appears to be
;;   the sweet spot.
(add-hook 'window-setup-hook #'doom-init-ui-h -100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Font-Lock Mode

;; Toggle Font-Lock mode in all buffers.
(global-font-lock-mode t)
;; Use the maximum decoration available.
(setq font-lock-maximum-decoration t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set the line spacing
(setq-default line-spacing 3) ; px

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Icons
;;   https://github.com/rainstormstudio/nerd-icons.el
;;   Nerd-icons.el is a library for easily using Nerd Font icons inside Emacs, an alternative to all-the-icons.
;;   https://github.com/rainstormstudio/nerd-icons.el

(use-package nerd-icons
  :commands (nerd-icons-octicon
             nerd-icons-faicon
             nerd-icons-flicon
             nerd-icons-wicon
             nerd-icons-mdicon
             nerd-icons-codicon
             nerd-icons-devicon
             nerd-icons-ipsicon
             nerd-icons-pomicon
             nerd-icons-powerline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mouse

;; middle-click paste at point, not at click
(setq mouse-yank-at-point t)

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
;; Highlights the current line

(use-package hl-line
  :hook (doom-first-buffer . global-hl-line-mode)
  :init
  (defvar global-hl-line-modes
    '(prog-mode
      text-mode
      conf-mode
      special-mode
      org-agenda-mode
      dired-mode
      )
    "What modes to enable `hl-line-mode' in.")
  :config
  ;; HACK I reimplement `global-hl-line-mode' so we can white/blacklist modes in
  ;;      `global-hl-line-modes' _and_ so we can use `global-hl-line-mode',
  ;;      which users expect to control hl-line in Emacs.
  ;;
  ;; (define-globalized-minor-mode GLOBAL-MODE MODE TURN-ON [KEY VALUE]... BODY...)
  ;;   Make a global mode GLOBAL-MODE corresponding to buffer-local minor MODE.
  ;;   TURN-ON is a function that will be called with no args in every buffer and that should try
  ;;   to turn MODE on if applicable for that buffer.
  (define-globalized-minor-mode global-hl-line-mode hl-line-mode
    (lambda ()
      (and (cond (hl-line-mode nil)
                 ((null global-hl-line-modes) nil)
                 ((eq global-hl-line-modes t))
                 ((eq (car global-hl-line-modes) 'not)
                  (not (derived-mode-p global-hl-line-modes)))
                 ((apply #'derived-mode-p global-hl-line-modes)))
           (hl-line-mode +1))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Fringes

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like git-gutter and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

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
;; Doom Modeline
;;  https://github.com/seagle0128/doom-modeline
;; for icon issues
;;   https://github.com/seagle0128/doom-modeline/issues/310
;;   To get font in ~/.local/share/fonts/ :  M-x nerd-icons-install-fonts
;;     before M-x all-the-icons-install-fonts

(use-package doom-modeline
  ;;s;; :ensure t
  ;; Fixme: check doom
  :hook (after-init . doom-modeline-mode)
  ;; :init (doom-modeline-mode 1)
  )

(use-package all-the-icons
  ;;s;; :ensure t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Powerline
;;  Enhance the powerline (information line below the buffer)
;;  https://github.com/milkypostman/powerline
;;  This is a proposed version 2.0 of the original Emacs Powerline which is a fork of vim-powerline.
;;  NOTE: This project is in maintenance mode and not currently being developed or maintained
;;  last 2022/01
;;
;;  require dnf install powerline-fonts

(use-package powerline
  :disabled
  :init
  (powerline-default-theme)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Line & Column Mode
;;   Show line/column in the modeline

(setq column-number-mode t)
;; (setq line-number-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ui)

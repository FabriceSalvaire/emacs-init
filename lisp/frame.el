;;; .el --- ... -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Theme -> ./themes
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Creating-Custom-Themes.html#Creating-Custom-Themes
;;   M-x list-faces-display
;;   M-x customize-create-theme
;;   M-x describe-face

(defun theme-light ()
  "Set basic light theme"
  (interactive)
  (progn
    (set-foreground-color "black")
    (set-background-color "white")

    ;; (set-foreground-color "#282c34")
    ;; (set-background-color "#f5f5f5")
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :config

  ;; Fullscreen
  (pcase window-system
    ('w32 (set-frame-parameter nil 'fullscreen 'fullboth))
    (_ (set-frame-parameter nil 'fullscreen 'maximized)))

  ;; Theme
  (load-theme 'fabrice-dark t)

  ;; Set the line spacing
  (setq-default line-spacing 3) ; px
  ;; Toggle Font-Lock mode in all buffers.
  (global-font-lock-mode t)
  ;; Use the maximum decoration available.
  (setq font-lock-maximum-decoration t)

  ;; Speedbar : buffer to navigate
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Speedbar.html
  ;; https://www.gnu.org/software/emacs/manual/html_node/speedbar/index.html#Top
  ;; M-x speedbar
  ;; (speedbar t)

  ;; Disable the tool bar
  (tool-bar-mode -1)
  )

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

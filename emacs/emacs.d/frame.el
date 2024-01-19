;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Frame settings
;;

;; Fullscreen
(pcase window-system
  ('w32 (set-frame-parameter nil 'fullscreen 'fullboth))
  (_ (set-frame-parameter nil 'fullscreen 'maximized)))

;; Speedbar : buffer to navigate
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Speedbar.html
;; https://www.gnu.org/software/emacs/manual/html_node/speedbar/index.html#Top
;; M-x speedbar
;; (speedbar t)

;; Disable the tool bar
(tool-bar-mode -1)

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
  (powerline-default-theme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Doom Modeline
;;  https://github.com/seagle0128/doom-modeline
;; for icon issues
;;   https://github.com/seagle0128/doom-modeline/issues/310
;;   To get font in ~/.local/share/fonts/ :  M-x nerd-icons-install-fonts
;;     before M-x all-the-icons-install-fonts

;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1))
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package all-the-icons
  :ensure t)

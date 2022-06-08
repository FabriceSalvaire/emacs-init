;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Frame settings
;;

;; Speedbar : buffer to navigate
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Speedbar.html
;; https://www.gnu.org/software/emacs/manual/html_node/speedbar/index.html#Top
;; M-x speedbar
;; (speedbar t)

;; Disable the tool bar
(tool-bar-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Powerline
;;  Enhance the powerline (information line below the buffer)
;;  https://github.com/milkypostman/powerline
;;  This is a proposed version 2.0 of the original Emacs Powerline which is a fork of vim-powerline.
;;  NOTE: This project is in maintenance mode and not currently being developed or maintained
;;  last 2022/01

(use-package powerline
  :init
  (powerline-default-theme))

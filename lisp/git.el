;;; .el --- ... -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; git-gutter
;;   https://github.com/emacsorphanage/git-gutter
;;
;; see ~/.config/doom-emacs/modules/ui/vc-gutter/config.el

(use-package git-gutter
  ;;s;; :ensure t
  ;; :hook (prog-mode . git-gutter-mode)

  :config
  (global-git-gutter-mode +1)
  )

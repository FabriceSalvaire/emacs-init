;;; dired.el --- Dired -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Dired
;;

;; https://www.emacswiki.org/emacs/DiredExtra#Dired_X
(add-hook 'dired-load-hook #'(lambda ()
                               (load "dired-x")
                               ;; Set dired-x global variables here.
                               ))

;; https://www.gnu.org/software/emacs/manual/html_node/dired-x/Omitting-Files-in-Dired.html
;; Fixme: symbol value as variable is void
;;(add-hook 'dired-mode-hook #'(lambda ()
;;                               ;; Set dired-x buffer-local variables here.
;;                               ;; dired-omit-toggle M-o
;;                               ;; (setq dired-omit-files-p t)
;;                               (setq dired-omit-files
;;                                     ;; do not wish to see `dot' files (files starting with a `.')
;;                                     ;;  '(dired-omit-files "^\\.$")
;;                                     (concat dired-omit-files "\\|^\\..+$"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; https://github.com/purcell/diredfl

(use-package diredfl
  ;;s;; :ensure t
  :hook (dired-mode . diredfl-mode)
  )

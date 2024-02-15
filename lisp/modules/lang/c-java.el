;;; .el --- ... -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-c-common-hook ()
  (progn
    (c-set-style "gnu")
      (setq-default c-basic-offset 2
                    indent-tabs-mode nil)
      ))

(defun c-set-indent-fabrice ()
  (interactive)
  (c-set-style "gnu")
  (c-set-offset 'brace-list-open 0)
  (c-set-offset 'inextern-lang 0)
  (message "Fabrice indentation."))

(defun c-set-qt-style ()
  (interactive)
  (c-set-style "qt-gnu")
  (message "Qt indentation."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; C
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Hideshow.html
  ;; Hideshow mode is a buffer-local minor mode that allows you to selectively display portions of
  ;; a program, which are referred to as blocks.
  :hook (c-mode-common . hs-minor-mode)

  :hook (c-mode-common . my-c-common-hook)
  ;; :hook (c-mode-common . c-set-indent-fabrice)

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; C++
   ;; modern-cpp-font-lock - Font-locking for Modern C++
   ;; https://github.com/ludwigpacifici/modern-cpp-font-lock
  :hook ('c++-mode . modern-c++-font-lock-mode)

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Java
   :hook (java-mode . (lambda ()
                        (setq c-basic-offset 2
                              indent-tabs-mode nil)))

  :config
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; C++
   (add-to-list 'auto-mode-alist '("\\.txx\\'" . c-mode))

   (c-add-style "qt-gnu"
                '("gnu"
                  (c-access-key . "\\<\\(signals\\|public\\|protected\\|private\\|public slots\\|protected slots\\|private slots\\):")
                  (c-basic-offset . 4)))
   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; clang-format

(use-package clang-format
  :defer t
  )

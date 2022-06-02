;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; C
;

(add-hook 'c-mode-common-hook 'hs-minor-mode)

(add-to-list 'auto-mode-alist '("\\.txx\\'" . c-mode))

(defun my-c-common-hook ()
    (progn
      (c-set-style "gnu")
      (setq-default c-basic-offset 2
		    indent-tabs-mode nil)
      ))
(add-hook 'c-mode-common-hook 'my-c-common-hook)

(defun c-set-indent-fabrice ()
  (interactive)
  (c-set-style "gnu")
  (c-set-offset 'brace-list-open 0)
  (c-set-offset 'inextern-lang 0)
  (message "Fabrice indentation."))
;; (add-hook 'c-mode-common-hook 'c-set-indent-fabrice)

(c-add-style "qt-gnu" '("gnu"
                        (c-access-key . "\\<\\(signals\\|public\\|protected\\|private\\|public slots\\|protected slots\\|private slots\\):")
                         (c-basic-offset . 4)))

(defun c-set-qt-style ()
  (interactive)
  (c-set-style "qt-gnu")
  (message "Qt indentation."))


;; modern-cpp-font-lock - Font-locking for Modern C++
;; https://github.com/ludwigpacifici/modern-cpp-font-lock
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

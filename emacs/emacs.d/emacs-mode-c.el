;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; C
;

(add-to-list 'auto-mode-alist '("\\.txx\\'" . c-mode))

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Hideshow.html
;; Hideshow mode is a buffer-local minor mode that allows you to selectively display portions of a program, which are referred to as blocks. 
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; modern-cpp-font-lock - Font-locking for Modern C++
;; https://github.com/ludwigpacifici/modern-cpp-font-lock
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(c-add-style "qt-gnu"
             '("gnu"
               (c-access-key . "\\<\\(signals\\|public\\|protected\\|private\\|public slots\\|protected slots\\|private slots\\):")
               (c-basic-offset . 4)))

(defun c-set-qt-style ()
  (interactive)
  (c-set-style "qt-gnu")
  (message "Qt indentation."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'c-mode-common-hook 'my-c-common-hook)
;; (add-hook 'c-mode-common-hook 'c-set-indent-fabrice)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FlyCheck - Syntax checking for GNU Emacs
;; http://www.flycheck.org/en/latest/
;;

;; (package-install 'flycheck)
;; (global-flycheck-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

;; (require 'flycheck-tip)
;; (define-key your-prog-mode (kbd "C-c C-n") 'flycheck-tip-cycle)

;; If you are still using flymake, you can use combined function that
;; show error by popup in flymake-mode or flycheck-mode.

;; (define-key global-map (kbd "C-0") 'error-tip-cycle-dwim)
;; (define-key global-map (kbd "C-9") 'error-tip-cycle-dwim-reverse)

;; If you build Emacs with D-Bus option, you may configure following setting.
;; This keeps the errors on notification area. Please check
;; ‘error-tip-notify-timeout’ to change limit of the timeout as well.

;; (setq error-tip-notify-keep-messages t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; C, C++ and Objective-C support for Flycheck, using Irony Mode
;; https://github.com/Sarcasm/flycheck-irony/
;;

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Flycheck Python
;;
;; https://github.com/lunaryorn/.emacs.d/blob/master/lisp/flycheck-virtualenv.el
;; http://liuluheng.github.io/wiki/public_html/Python/flycheck-pylint-emacs-with-python.html
;; http://emacs.stackexchange.com/questions/5460/starting-a-virtualenv-for-emacs-to-use
;;
;; /!\ pylint must be found !
;;


;; WRONG setq(flycheck-python-pylint-executable "/usr/bin/python3-pylint")


;; (add-hook 'python-mode-hook (lambda ()
;; 			      (flycheck-mode 1)
;; 			      (semantic-mode 1)
;; 			      (setq flycheck-checker 'python-pylint
;; 				    flycheck-checker-error-threshold 900
;; 				    flycheck-pylintrc "~/.pylintrc")))



;; (defun flycheck-python-setup ()
;;   (flycheck-mode))
;; (add-hook 'python-mode-hook #'flycheck-python-setup)



;; (defun set-flychecker-executables ()
;;   "Configure virtualenv for flake8 and lint."
;;   (when (get-current-buffer-flake8)
;; (flycheck-set-checker-executable (quote python-flake8)
;; 				 (get-current-buffer-flake8)))
;; (when (get-current-buffer-pylint)
;;   (flycheck-set-checker-executable (quote python-pylint)
;; 				   (get-current-buffer-pylint))))
;; (add-hook 'flycheck-before-syntax-check-hook
;; 	  #'set-flychecker-executables 'local)



;; (declare-function python-shell-calculate-exec-path "python")

;; (defun flycheck-virtualenv-executable-find (executable)
;;   "Find an EXECUTABLE in the current virtualenv if any."
;;   (if (bound-and-true-p python-shell-virtualenv-root)
;;       (let ((exec-path (python-shell-calculate-exec-path)))
;;         (executable-find executable))
;;     (executable-find executable)))

;; (defun flycheck-virtualenv-setup ()
;;   "Setup Flycheck for the current virtualenv."
;;   (setq-local flycheck-executable-find #'flycheck-virtualenv-executable-find))

;; (add-hook 'python-mode-hook #'flycheck-virtualenv-setup)

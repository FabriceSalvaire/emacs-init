;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

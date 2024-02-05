;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FlyCheck - Syntax checking for GNU Emacs
;;   http://www.flycheck.org/en/latest

(use-package flycheck
  ;:disabled
  :ensure t

  :init
  (global-flycheck-mode)
  (flycheck-pos-tip-mode)

  ;; C, C++ and Objective-C support for Flycheck, using Irony Mode
  ;;   https://github.com/Sarcasm/flycheck-irony/
  ;;! :hook (flycheck-mode . flycheck-irony-setup)

  ;; JavaScript
  ;;   cf. supra
  )

;; A minor-mode for Flycheck which colors the mode line according to the Flycheck state of the current buffer.
;; https://github.com/flycheck/flycheck-color-mode-line
(use-package flycheck-color-mode-line
  :after (flycheck)
  :hook (flycheck-mode . flycheck-color-mode-line-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; customize flycheck temp file prefix
;; (setq-default flycheck-temp-prefix ".flycheck")

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
;; JavaScript
;;   http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;;

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
	      (append flycheck-disabled-checkers
		      '(json-jsonlist)))

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
	      (append flycheck-disabled-checkers
		      '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
;;   This code looks for a node_modules directory in any parent of the
;;   buffer's directory and configures Flycheck to use an eslint
;;
;;   executable from that directory if any exists.
;;  (locate-dominating-file FILE NAME)
;;      Look up the directory hierarchy from FILE for a directory containing NAME.
;;  expand-file-name filename &optional directory
;;      converts filename to an absolute file name
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

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

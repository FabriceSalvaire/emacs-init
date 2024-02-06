;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; JavaScript
;;   http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FlyCheck - Syntax checking for GNU Emacs
;;   http://www.flycheck.org/en/latest

(use-package flycheck
  ;:disabled
  :ensure t

  :init
  (global-flycheck-mode)
  ;; A minor mode to show Flycheck error messages in a popup.
  (flycheck-pos-tip-mode)

  ;; C, C++ and Objective-C support for Flycheck, using Irony Mode
  ;;   https://github.com/Sarcasm/flycheck-irony/
  ;;! :hook (flycheck-mode . flycheck-irony-setup)

  :config
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Javascript / JSON
  ;;   disable json-jsonlist checking for json files
  ;;   disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(
			  json-jsonlist
			  javascript-jshint
			  )))
  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  )

;; A minor-mode for Flycheck which colors the mode line according to the Flycheck state of the current buffer.
;; https://github.com/flycheck/flycheck-color-mode-line
(use-package flycheck-color-mode-line
  :after (flycheck)
  :hook (flycheck-mode . flycheck-color-mode-line-mode)
  )

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; eglot — A client for Language Server Protocol servers
;;  [Eglot](https://www.gnu.org/software/emacs/manual/html_mono/eglot.html)
;;  [joaotavora/eglot: A client for Language Server Protocol servers](https://github.com/joaotavora/eglot)
;;  in Emacs 29
;;  [How to configure eglot to use flycheck?](https://github.com/joaotavora/eglot/issues/42)
;;  [Migrating from LSP-Mode to Eglot](https://andreyor.st/posts/2023-09-09-migrating-from-lsp-mode-to-eglot/)
;;
;;  eglot-... commands must be available when eglot is enabled
;;  M-x eglot-show-workspace-configuration

(use-package eglot
  ; :disabled
  :ensure t

  :commands eglot
  :hook ((python-mode python-ts-mode) . eglot-ensure)

  :config
  ;; Astro
  (add-to-list 'eglot-server-programs
	       ;; Fixme: path
               '(astro-ts-mode . ("/home/fabrice/astro-test/homepage/node_modules/.bin/astro-ls" "--stdio"
				  :initializationOptions
				  (:typescript (:tsdk "/home/fabrice/astro-test/homepage/node_modules/typescript/lib")))))
  ;; Fixme: doesn't work ?
  ;; (setq-default eglot-workspace-configuration
  ;;               '(:pylsp (:plugins (:jedi_completion (:include_params t :fuzzy t)
  ;;                                   :pylint (:enabled :json-false)))
  ;; 			 :gopls (:usePlaceholders t)))

  :custom
  (eglot-autoshutdown t)

  :init
  ;; auto start eglot for astro-mode
  (add-hook 'astro-ts-mode-hook 'eglot-ensure)
  )

;; see eglot.el
;; (defvar eglot-server-programs `(...
;;                                 ((python-mode python-ts-mode)
;;                                  . ,(eglot-alternatives
;;                                      '("pylsp" "pyls"
;; 				       ("pyright-langserver" "--stdio")
;; 				       "jedi-language-server" "ruff-lsp")))
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                `((python-mode python-ts-mode) .
;; 		 ,(eglot-alternatives '( "pylsp"
;; 					 "pyls"
;; 					 ("poetry" "run" "pyright-langserver" "--stdio")
;; 					 ("pyright-langserver" "--stdio")
;; 					 "jedi-language-server")))))

;; [intramurz/flycheck-eglot](https://github.com/intramurz/flycheck-eglot)
;; Flycheck support for eglot
(use-package flycheck-eglot
  ; :disabled
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; LSP — Language Server Protocol
;;   https://emacs-lsp.github.io
;;   https://www.mattduck.com/lsp-python-getting-started.html

(use-package lsp-mode
  :disabled ; !!!
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  ;; Project errors on modeline
  ;;  https://emacs-lsp.github.io/lsp-mode/page/main-features/#project-errors-on-modeline
  (setq lsp-modeline-diagnostics-scope :workspace)

  :config
  ;; cf. M-x lsp-doctor
  (setq gc-cons-threshold (* 100 1024 1024) ;; 100 MB
	read-process-output-max (* 1024 1024) ;; 1 MB
	lsp-idle-delay 0.1 ;; clangd is fast
	)

  ;; The default configuration sources are pycodestyle and pyflakes.
  ;;   cf. https://github.com/python-lsp/python-lsp-server
  ;;   else flake8 config is ignored
  (setq
   ;; https://emacs-lsp.github.io/lsp-mode/page/lsp-pylsp/
   lsp-pylsp-plugins-autopep8-enabled nil
   lsp-pylsp-plugins-black-enabled nil
   lsp-pylsp-plugins-flake8-enabled t
   lsp-pylsp-plugins-mccabe-enabled nil
   lsp-pylsp-plugins-pycodestyle-enabled nil
   lsp-pylsp-plugins-pydocstyle-enabled nil
   lsp-pylsp-plugins-pyflakes-enabled nil
   lsp-pylsp-plugins-pylint-enabled t
   ;; lsp-pylsp-configuration-sources ["flake8"]
   ;;
   lsp-pyls-plugins-pycodestyle-enabled nil
   lsp-pyls-plugins-mccabe-enabled nil
   lsp-pyls-plugins-pyflakes-enabled nil
   lsp-pyls-plugins-flake8-enabled t
   lsp-pyls-configuration-sources ["flake8"]
   )
  ;; lsp-diagnostics-provider :auto

  ;; https://emacs-lsp.github.io/lsp-mode/page/troubleshooting/
  ;; (setq lsp-log-io t)

  :hook (
         (c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))

  :commands lsp
  )

(use-package lsp-ui
  :commands lsp-ui-mode
  )
;; if you are helm user
;;   (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol
  )
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  )

;; debugger
;; https://github.com/emacs-lsp/dap-mode
(use-package dap-mode)
(use-package dap-cpptools)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; multiple-cursors — Multiple cursors for emacs
;;   [magnars/multiple-cursors.el](https://github.com/magnars/multiple-cursors.el)

(require 'multiple-cursors)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; treesit — parsing library
;;   [Tree-sitter](https://tree-sitter.github.io/tree-sitter/)
;;   [How to Get Started with Tree-Sitter](https://www.masteringemacs.org/article/how-to-get-started-tree-sitter)

(use-package treesit
  ; :disabled
  :ensure nil
  :when (treesit-available-p)
  :init
  ;; (mapc #'treesit-install-language-grammar '(astro css tsx))
  (setq treesit-language-source-alist
	'(
	  (astro "https://github.com/virchau13/tree-sitter-astro")
	  (bash "https://github.com/tree-sitter/tree-sitter-bash")
	  (cmake "https://github.com/uyha/tree-sitter-cmake")
	  (css "https://github.com/tree-sitter/tree-sitter-css")
	  (elisp "https://github.com/Wilfred/tree-sitter-elisp")
	  (go "https://github.com/tree-sitter/tree-sitter-go")
	  (html "https://github.com/tree-sitter/tree-sitter-html")
	  (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	  (json "https://github.com/tree-sitter/tree-sitter-json")
	  (make "https://github.com/alemuller/tree-sitter-make")
	  (markdown "https://github.com/ikatyang/tree-sitter-markdown")
	  (python "https://github.com/tree-sitter/tree-sitter-python")
	  (scss "https://github.com/serenadeai/tree-sitter-scss")
	  (toml "https://github.com/tree-sitter/tree-sitter-toml")
	  (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	  (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	  (yaml "https://github.com/ikatyang/tree-sitter-yaml")
	  ))
  (setq treesit-font-lock-level 3)
  ; see treesit-auto
  (setq major-mode-remap-alist
        '(
          (bash-mode . bash-ts-mode)
          (c++-mode . c++-ts-mode)
          (c-mode . c-ts-mode)
          (c-or-c++-mode . c-or-c++-ts-mode)
          (cmake-mode . cmake-ts-mode)
          (css-mode . css-ts-mode)
          (javas-mode . java-ts-mode)
          (javascript-mode . javascript-ts-mode)
          (js-json-mode . js-json-ts-mode)
          (python-mode . python-ts-mode)
          (yaml-mode . yaml-ts-mode)
          ))
  :config
  ;; [mickeynp/combobulate: Structured Editing and Navigation in Emacs with Tree-Sitter](https://github.com/mickeynp/combobulate)
  ;; [Combobulate: Structured Movement and Editing with Tree-Sitter](https://www.masteringemacs.org/article/combobulate-structured-movement-editing-treesitter)
  ;; Do not forget to customize Combobulate to your liking:
  ;;
  ;;  M-x customize-group RET combobulate RET
  ;;
  (use-package combobulate
    :preface
    ;; You can customize Combobulate's key prefix here.
    ;; Note that you may have to restart Emacs for this to take effect!
    ; (setq combobulate-key-prefix "C-c o")

    ;; Optional, but recommended.
    ;;
    ;; You can manually enable Combobulate with `M-x
    ;; combobulate-mode'.
    ; :hook ((python-ts-mode . combobulate-mode)
    ;        (js-ts-mode . combobulate-mode)
    ;        (css-ts-mode . combobulate-mode)
    ;        (yaml-ts-mode . combobulate-mode)
    ;        (json-ts-mode . combobulate-mode)
    ;        (typescript-ts-mode . combobulate-mode)
    ;        (tsx-ts-mode . combobulate-mode))
    ;; Amend this to the directory where you keep Combobulate's source
    ;; code.
    :load-path ("/home/common/emacs/checkout/combobulate"))
  )

;; [renzmann/treesit-auto: Automatic installation, usage, and fallback for tree-sitter major modes in Emacs 29](https://github.com/renzmann/treesit-auto)
;; M-x treesit-auto-install-all
(use-package treesit-auto
  ; :disabled
  :config
  (global-treesit-auto-mode)
  ;;
  ;; (let ((astro-recipe (make-treesit-auto-recipe
  ;;                      :lang 'astro
  ;;                      :ts-mode 'astro-ts-mode
  ;;                      :url "https://github.com/virchau13/tree-sitter-astro"
  ;;                      :revision "master"
  ;;                      :source-dir "src")))
  ;;   (add-to-list 'treesit-auto-recipe-list astro-recipe))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; show-paren-mode — highlight matching delimiters
;;   https://www.emacswiki.org/emacs/ShowParenMode

(use-package paren
  :custom
  (show-paren-delay 0.1)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)

  :init
  ;; global
  (show-paren-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; corfu — COmpletion in Region FUnction
;; [minad/corfu](https://github.com/minad/corfu)

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; company-mode
;;   Company is a text completion framework for Emacs
;;   https://company-mode.github.io
;;   https://company-mode.github.io/manual

(use-package company
  :disabled ; !!!
  :custom
  (company-idle-delay 0) ; default 0.2
  (company-minimum-prefix-length 1) ; default 3

  :init
  (global-company-mode)
  ;; (add-hook 'after-init-hook 'global-company-mode)
  ;; see candidates are automatically lowercased
  ;;  https://github.com/company-mode/company-mode/issues/14
  (add-to-list 'company-dabbrev-code-modes 'markdown-mode)
  (add-to-list 'company-dabbrev-code-modes 'c++-mode)
  )

;; company-box — A company front-end with icons
;;   https://github.com/sebastiencs/company-box/
(use-package company-box
  :disabled
  :hook (company-mode . company-box-mode)
  )

;; company-c-headers — Auto-completion for C/C++ headers using Company
;;   https://github.com/randomphrase/company-c-headers
;;! (eval-after-load 'company
;;!   '(add-to-list 'company-backends 'company-c-headers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Irony
;;   A C/C++ minor mode powered by libclang
;;   https://github.com/Sarcasm/irony-mode
;;   https://github.com/Sarcasm/flycheck-irony
;;

(if nil
    (progn
      (use-package company-irony-c-headers)
      ;; Load with `irony-mode` as a grouped backend
      (eval-after-load 'company
        '(add-to-list 'company-backends '(company-irony-c-headers company-irony)))

      (add-hook 'c++-mode-hook 'irony-mode)
      (add-hook 'c-mode-hook 'irony-mode)
      ;; (add-hook 'objc-mode-hook 'irony-mode)

      ;; replace the `completion-at-point' and `complete-symbol' bindings in
      ;; irony-mode's buffers by irony-mode's function
      (defun my-irony-mode-hook ()
        (define-key irony-mode-map [remap completion-at-point]
          'irony-completion-at-point-async)
        (define-key irony-mode-map [remap complete-symbol]
          'irony-completion-at-point-async))
      (add-hook 'irony-mode-hook 'my-irony-mode-hook)
      (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
      (add-hook 'irony-mode-hook 'irony-eldoc)
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Yasnippet
;;  https://github.com/joaotavora/yasnippet

;; yas/insert-snippet: C-c & C-s
;; /home/fabrice/.emacs.d/snippets ->

(use-package yasnippet
  :config
  (yas/global-mode 1)

  ;; (setq yas/snippet-dirs (concat local_emacs_d_path "snippets"))
  ;; (load "snippet-bundel.el" t t)

  ;; http://sethlakowske.com/why-i-use-emacs/fix-yasnippet-and-autocomplete-tab-key-collision/
  ;; Remove Yasnippet's default tab key binding
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; Set Yasnippet's key binding to shift+tab
  (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
  ;; Alternatively use Control-c + tab
  ;; (define-key yas-minor-mode-map (kbd "\C-c TAB") 'yas-expand)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; clang-format

(use-package clang-format)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Projectile
;;   https://github.com/bbatsov/projectile
;;   https://docs.projectile.mx/projectile/index.html
;;
;;  dnf install fd-find the_silver_searcher ripgrep
;;    fd — find entries in the filesystem
;;    ag — The Silver Searcher
;;    rg — recursively search the current directory for lines matching a pattern

; (add-to-list 'package-pinned-packages '(projectile . "melpa-stable") t)

(use-package projectile
  :ensure t

  :init
  (projectile-mode +1)

  ;; https://github.com/anshulverma/projectile-speedbar
  ;; versus treemacs-projectile
  ;; (use-package projectile-speedbar)

  :config
  (setq
   ;; Recursive discovery is configured by specifying the search depth in a cons cell
   projectile-project-search-path '(("~/__projects__/" . 1))

   ;; projectile-sort-order 'default
   projectile-sort-order 'recently-active

   projectile-completion-system 'ivy
   )

  ;; https://docs.projectile.mx/projectile/projects.html
  (projectile-register-project-type 'CMakeGit
                                    '("CMakeLists.txt" ".git")
                                    :project-file "CMakeLists.txt"
                                    :compile ""
                                    :test ""
                                    :run ""
                                    :test-suffix ""
                                    )

  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  )

;; https://github.com/ericdanan/counsel-projectile for Ivy integration
(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode 1))

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
  :hook ((python-mode python-ts-mode)
	 . eglot-ensure)
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
  (global-flycheck-eglot-mode 1))

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

;; cf. M-x lsp-doctor
(setq gc-cons-threshold (* 100 1024 1024) ;; 100mb
      read-process-output-max (* 1024 1024) ;; 1mb
      lsp-idle-delay 0.1 ;; clangd is fast
      )

(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
;;   (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; debugger
;; https://github.com/emacs-lsp/dap-mode
(use-package dap-mode)
(use-package dap-cpptools)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Auto-Complete

;; (require 'auto-complete)
;; (global-auto-complete-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Anything

;; (add-to-list 'load-path (concat local_emacs_site_lisp_path "anything"))

;; (require 'anything)
;; (require 'anything-ipython)
;; (when (require 'anything-show-completion nil t)
;;   (use-anything-show-completion 'anything-ipython-complete
;;                              '(length initial-pattern)))

;; (require 'anything-config)
;; (require 'anything-match-plugin)
;; (global-set-key "\C-ca" 'anything)
;; (global-set-key "\C-ce" 'anything-for-files)

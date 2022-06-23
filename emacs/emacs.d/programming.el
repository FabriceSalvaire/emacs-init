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
;; company-mode
;;   Company is a text completion framework for Emacs
;;   https://company-mode.github.io
;;   https://company-mode.github.io/manual

(use-package company
  :custom
  (company-idle-delay 0) ; default 0.2
  (company-minimum-prefix-length 1) ; default 3

  :init
  (global-company-mode)
  ;; (add-hook 'after-init-hook 'global-company-mode)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; LSP — Language Server Protocol
;;   https://emacs-lsp.github.io
;;   https://www.mattduck.com/lsp-python-getting-started.html

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  ;; Project errors on modeline
  (setq lsp-modeline-diagnostics-scope :workspace)

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
;; 				'(length initial-pattern)))

;; (require 'anything-config)
;; (require 'anything-match-plugin)
;; (global-set-key "\C-ca" 'anything)
;; (global-set-key "\C-ce" 'anything-for-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; use-package — A use-package declaration for simplifying your .emacs
;;   [jwiegley/use-package](https://github.com/jwiegley/use-package)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package foo
  ;; to diable
  :diabled

  ;; explicit lazy loading
  :defer t
  ;; force loading to occur immediately and not establish an autoload for the bound key
  :demand t

  ;; loading order
  :after (ivy hydra))

  ;; to create an autoload for a command
  :commands ace-jump-mode

  ;; to bind a key (and create an autoload)
  :bind ("C-." . ace-jump-mode))
  :bind (("M-o l" . highlight-lines-matching-regexp)
         ("M-o r" . highlight-regexp)
         ("M-o w" . highlight-phrase)
         ("M-<f5>" . helm-find-files)
         ([f10] . helm-buffers-list)
         ([S-f10] . helm-recentf))

  :bind-keymap
  ("C-c p" . projectile-command-map)

  ;; to establish a deferred binding within the auto-mode-alist and interpreter-mode-alist variables
  :mode "\\.rb\\'"
  :interpreter "ruby"
  ;; The package is "python" but the mode is "python-mode"
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

  ;; to cause certain function to be run if the beginning of a file matches a given regular expression
  :magic ("%PDF" . pdf-view-mode)

  ;; to ad functions onto package hooks
  :hook prog-mode
  :hook (prog-mode text-mode)
  ;; same as
  ;;   :hook (prog-mode . ace-jump-mode)
  ;; same as
  ;;   :commands ace-jump-mode
  ;;   :init
  ;;   (add-hook 'prog-mode-hook #'ace-jump-mode)

  ;; to customize variables
  :custom
  (comint-buffer-maximum-size 20000 "Increase comint buffer size.")

  ;; can be used to establish function and variable definitions that will
  ;;   1) make the byte-compiler happ
  :preface

  ;; to execute code before a package is loaded
  :init
  (setq foo-variable t)

  ;; to execute code after a package is loaded
  :config

  ;; prevent loading if dependencies are missing
  :requires foo

  ;; diminished
  :diminish
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Getting started

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

(use-package foo)


(use-package foo
  :init
  (setq foo-variable t))


(use-package foo
  :init
  (setq foo-variable t)
  :config
  (foo-mode 1))


(use-package color-moccur
  :commands (isearch-moccur isearch-all)
  :bind (("M-s O" . moccur)
         :map isearch-mode-map
         ("M-o" . isearch-moccur)
         ("M-O" . isearch-moccur-all))
  :init
  (setq isearch-lazy-highlight t)
  :config
  (use-package moccur-edit))


(use-package org-crypt
  :autoload org-crypt-use-before-save-magic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key-binding

(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode))


(use-package ace-jump-mode
  :commands ace-jump-mode
  :init
  (bind-key "C-." 'ace-jump-mode))


(use-package hi-lock
  :bind (("M-o l" . highlight-lines-matching-regexp)
         ("M-o r" . highlight-regexp)
         ("M-o w" . highlight-phrase)))


(use-package avy
  :bind ("C-:" ("Jump to char" . avy-goto-char)
         "M-g f" ("Jump to line" . avy-goto-line)))


(use-package helm
  :bind (("M-x" . helm-M-x)
         ("M-<f5>" . helm-find-files)
         ([f10] . helm-buffers-list)
         ([S-f10] . helm-recentf)))


(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))


;; Binding to keymaps

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map))


;; Binding within local keymaps

(use-package helm
  :bind (:map helm-command-map
         ("C-c h" . helm-execute-persistent-action)))


(use-package term
  :bind (("C-c t" . term)
         :map term-mode-map
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)
         :map term-raw-map
         ("M-o" . other-window)
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)))


;; Binding to repeat-maps

(use-package git-gutter+
  :bind
  (:repeat-map git-gutter+-repeat-map
   ("n" . git-gutter+-next-hunk)
   ("p" . git-gutter+-previous-hunk)
   ("s" . git-gutter+-stage-hunks)
   ("r" . git-gutter+-revert-hunk)))


(use-package git-gutter+
  :bind
  (:repeat-map my/git-gutter+-repeat-map
   ("n" . git-gutter+-next-hunk)
   ("p" . git-gutter+-previous-hunk)
   ("s" . git-gutter+-stage-hunks)
   ("r" . git-gutter+-revert-hunk)
   :exit
   ("c" . magit-commit-create)
   ("C" . magit-commit)
   ("b" . magit-blame)))


(use-package git-gutter+
  :bind
  (:repeat-map my/git-gutter+-repeat-map
   :exit
   ("c" . magit-commit-create)
   ("C" . magit-commit)
   ("b" . magit-blame)
   :continue
   ("n" . git-gutter+-next-hunk)
   ("p" . git-gutter+-previous-hunk)
   ("s" . git-gutter+-stage-hunks)
   ("r" . git-gutter+-revert-hunk)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes and interpreters

(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby")

;; The package is "python" but the mode is "python-mode":
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))


(use-package ace-jump-mode
  :defer t
  :init
  (autoload 'ace-jump-mode "ace-jump-mode" nil t)
  (bind-key "C-." 'ace-jump-mode))

(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode))
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks

(use-package company
  :hook (prog-mode . company-mode))

(use-package company
  :commands company-mode
  :init
  (add-hook 'prog-mode-hook #'company-mode))


(use-package company
  :hook ((prog-mode text-mode) . company-mode))

(use-package company
  :hook ((prog-mode . company-mode)
         (text-mode . company-mode)))

(use-package company
  :hook (prog-mode . company-mode)
  :hook (text-mode . company-mode))

(use-package company
  :hook
  (prog-mode . company-mode)
  (text-mode . company-mode))

(use-package company
  :commands company-mode
  :init
  (add-hook 'prog-mode-hook #'company-mode)
  (add-hook 'text-mode-hook #'company-mode))


;; DOES NOT WORK
(use-package ace-jump-mode
  :hook (prog-mode-hook . ace-jump-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package customization

;; Customizing variables

(use-package comint
  :custom
  (comint-buffer-maximum-size 20000 "Increase comint buffer size.")
  (comint-prompt-read-only t "Make the prompt read only."))


;; Customizing faces

(use-package eruby-mode
  :custom-face
  (eruby-standard-face ((t (:slant italic)))))

(use-package example
  :custom-face
  (example-1-face ((t (:foreground "LightPink"))))
  (example-2-face ((t (:foreground "LightGreen"))) face-defspec-spec))

(use-package zenburn-theme
  :preface
  (setq my/zenburn-colors-alist
        '((fg . "#DCDCCC") (bg . "#1C1C1C") (cyan . "#93E0E3")))
  :custom-face
  (region ((t (:background ,(alist-get my/zenburn-colors-alist 'cyan)))))
  :config
  (load-theme 'zenburn t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditional loading

(use-package edit-server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))


(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))


(use-package ess-site
  :disabled
  :commands R)


;; Conditional loading before :preface

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))


;; Loading packages in sequence

(use-package hydra
  :load-path "site-lisp/hydra")

(use-package ivy
  :load-path "site-lisp/swiper")

(use-package ivy-hydra
  :after (ivy hydra))

;; :after (foo bar)
;; :after (:all foo bar)
;; :after (:any foo bar)
;; :after (:all (:any foo bar) (:any baz quux))
;; :after (:any (:all foo bar) (:all baz quux))


;; Prevent loading if dependencies are missing

(use-package abbrev
  :requires foo)

(use-package abbrev
  :if (featurep 'foo))

(use-package abbrev
  :requires (foo bar baz))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Byte-compiling your .emacs

(use-package texinfo
  :defines texinfo-section-list
  :commands texinfo-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.texi$" . texinfo-mode)))


(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :functions inf-ruby-keys
  :config
  (defun my-ruby-mode-hook ()
    (require 'inf-ruby)
    (inf-ruby-keys))

  (add-hook 'ruby-mode-hook 'my-ruby-mode-hook))


;; Prevent a package from loading at compile-time

(use-package foo
  :no-require t
  :config
  (message "This is evaluated when `foo' is loaded"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extending the load-path

(use-package ess-site
  :load-path "site-lisp/ess/lisp/"
  :commands R)


(eval-and-compile
  (defun ess-site-load-path ()
    (shell-command "find ~ -path ess/lisp")))

(use-package ess-site
  :load-path (lambda () (list (ess-site-load-path)))
  :commands R)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Catching errors during use-package expansion

(use-package example
  ;; Note that errors are never trapped in the preface, since doing so would
  ;; hide definitions from the byte-compiler.
  :preface (message "I'm here at byte-compile and load time.")
  :init (message "I'm always here at startup")
  :config
  (message "I'm always here after the package is loaded")
  (error "oops")
  ;; Don't try to (require 'example), this is just an example!
  :no-require t
  :catch (lambda (keyword err)
           (message (error-message-string err))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Diminishing and delighting minor mode

(use-package abbrev
  :diminish abbrev-mode
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))


;; Don't show anything for rainbow-mode.
(use-package rainbow-mode
  :delight)

;; Don't show anything for auto-revert-mode, which doesn't match
;; its package name.
(use-package autorevert
  :delight auto-revert-mode)

;; Remove the mode name for projectile-mode, but show the project name.
(use-package projectile
  :delight '(:eval (concat " " (projectile-project-name))))

;; Completely hide visual-line-mode and change auto-fill-mode to " AF".
(use-package emacs
  :delight
  (auto-fill-function " AF")
  (visual-line-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package installation

(use-package magit
  :ensure t)

(use-package tex
  :ensure auctex)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;;; .el --- ... -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Combobulate: Structured Editing and Navigation in Emacs with Tree-Sitter
;;   https://github.com/mickeynp/combobulate
;; [Combobulate: Structured Movement and Editing with Tree-Sitter](https://www.masteringemacs.org/article/combobulate-structured-movement-editing-treesitter)
;; Do not forget to customize Combobulate to your liking:
;;
;;  M-x customize-group RET combobulate RET

(use-package combobulate
  :load-path (lambda () (concat local_checkout_path "combobulate"))

  :preface
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  ;; (setq combobulate-key-prefix "C-c o")

  ;; Optional, but recommended.
  ;;
  ;; You can manually enable Combobulate with `M-x combobulate-mode'.
                                        ; :hook ((python-ts-mode . combobulate-mode)
                                        ;        (js-ts-mode . combobulate-mode)
                                        ;        (css-ts-mode . combobulate-mode)
                                        ;        (yaml-ts-mode . combobulate-mode)
                                        ;        (json-ts-mode . combobulate-mode)
                                        ;        (typescript-ts-mode . combobulate-mode)
                                        ;        (tsx-ts-mode . combobulate-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; corfu — COmpletion in Region FUnction
;;   [minad/corfu](https://github.com/minad/corfu)

(use-package corfu
  :ensure t

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

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can be used globally (M-/).
  ;; See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode)
  )

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
(setq tab-always-indent 'complete)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; company-mode
;;   Company is a text completion framework for Emacs
;;   https://company-mode.github.io
;;   https://company-mode.github.io/manual

(use-package company
  :disabled

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

;; Fixme:
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
  :ensure t

  :config
  (yas/global-mode 1)

  (setq yas-snippet-dirs (concat local_path_prefix "snippets"))
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

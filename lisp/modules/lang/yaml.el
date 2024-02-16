;;; lang/yaml.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; YAML
;;   https://github.com/yoshiki/yaml-mode

(use-package yaml-mode
  :mode "\\.yml\\'"
  ;; :mode "Procfile\\'"
  :init
  ;; (when (modulep! +lsp)
  ;;   (add-hook 'yaml-mode-local-vars-hook #'lsp! 'append))
  ;; (when (modulep! +tree-sitter)
  ;;   (add-hook 'yaml-mode-local-vars-hook #'tree-sitter! 'append))
  :config
  ;; Fixme: check indentation setup
  (setq-hook! 'yaml-mode-hook tab-width yaml-indent-offset)
  )

;; Unlike python-mode, this mode follows the Emacs convention of not
;; binding the ENTER key to `newline-and-indent'.  To get this
;; behavior, add the key definition to `yaml-mode-hook':
;;
;;    (add-hook 'yaml-mode-hook
;;     '(lambda ()
;;        (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

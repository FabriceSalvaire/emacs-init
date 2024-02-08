;;; .el --- ... -*- lexical-binding: t; -*-

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

  ; See treesit-auto
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

  :custom
  (treesit-font-lock-level 3)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; treesit-auto: Automatic installation, usage, and fallback for tree-sitter major modes in Emacs 29
;;   https://github.com/renzmann/treesit-auto
;;   M-x treesit-auto-install-all

(use-package treesit-auto
  ; :disabled
  ;;s;; :ensure t

  :when (treesit-available-p)

  :config
  (global-treesit-auto-mode)

  ;; (let ((astro-recipe (make-treesit-auto-recipe
  ;;                      :lang 'astro
  ;;                      :ts-mode 'astro-ts-mode
  ;;                      :url "https://github.com/virchau13/tree-sitter-astro"
  ;;                      :revision "master"
  ;;                      :source-dir "src")))
  ;;   (add-to-list 'treesit-auto-recipe-list astro-recipe))
  )

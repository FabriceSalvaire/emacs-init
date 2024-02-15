;;; .el --- ... -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Python
;;

;; Use C-h m to check which mode is loaded

;; https://github.com/tree-sitter/tree-sitter-python
;; https://gist.github.com/habamax/290cda0e0cdc6118eb9a06121b9bc0d7
;
;; builtin
;;   https://github.com/fgallina/python.el
;;   Fabián Ezequiel Gallina
;;   last Nov 23, 2014

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; python-mode remap C-<delete> C-<backspace>
;; (setq python-mode-map
;;       (let ((map (make-sparse-keymap)))
;;         (define-key map [(control backspace)] 'py-hungry-delete-backwards)
;;         (define-key map [(control c) (delete)] 'py-hungry-delete-forward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fixme: sometimes python-mode is not loaded
;; check Python menu
;; (require 'python-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  ;; :bind (:map python-mode-map
  ;;             ("C-<delete>" . py-kill-word)
  ;;             ("C-<backspace>" . py-backward-kill-word)
  ;;        :map python-ts-mode-map
  ;;             ("C-<delete>" . py-kill-word)
  ;;             ("C-<backspace>" . py-backward-kill-word))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Company JEDI
  ;;   https://github.com/syohex/emacs-company-jedi
  :hook (python-mode . (lambda ()
                         (add-to-list 'company-backends 'company-jedi)
                         ;; https://github.com/syl20bnr/spacemacs/issues/15137
                         ;; don't work ??? see lsp config
                         ;; (setq flycheck-checker 'python-flake8)
                         ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; python-mode MELPA
;;   https://gitlab.com/python-mode-devs/python-mode

(use-package python-mode
  :disabled ; !!!
  :custom
  (py-outline-minor-mode-p 'nil)
  ; don't work ???
  (py-underscore-word-syntax-p 'nil)
  ; :config
  ;(add-hook 'python-mode-hook ...)
  :bind (:map python-mode-map
              ("C-<delete>" . py-kill-word)
              ("C-<backspace>" . py-backward-kill-word))
  )

;; (setq interpreter-mode-alist
;;       (cons '("python" . python-mode)
;;             interpreter-mode-alist))

;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; (autoload 'python-mode "python-mode" "Python Mode." t)
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; (add-to-list 'interpreter-mode-alist '("python" . python-mode))
;;
;; (global-set-key [M-right] 'py-forward-into-nomenclature)
;; (global-set-key [M-left] 'py-backward-into-nomenclature)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; pyvenv
;;  https://github.com/jorgenschaefer/pyvenv

;; The main entry points are `pyvenv-activate', which queries the user for a virtual environment
;; directory to activate, and `pyvenv-workon', which queries for a virtual environment in
;; $WORKON_HOME (from virtualenvwrapper.sh).

;; If you want your inferior Python processes to be restarted automatically when you switch your
;; virtual environment, add `pyvenv-restart-python' to `pyvenv-post-activate-hooks'.

(use-package pyvenv
  :demand t
  :config
  ;; Default venv
  ;; (setq pyvenv-workon "emacs")
  (setq pyvenv-workon "py311")
  ;; Automatically use pyvenv-workon via dir-locals
  ;; (pyvenv-tracking-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The Emacs IPython Notebook
;;   http://millejoh.github.io/emacs-ipython-notebook/#quick-try

(use-package ein
  :defer t
  )
;; (use-package ein-loaddefs)
;; Fixme: package source ?
;; (use-package ein-notebook
;;   :defer t
;;   )
;; (use-package ein-subpackages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Live Coding in Python
;;   http://donkirkby.github.io/live-py-plugin/
;;
;;   To use it, open a Python file and run M-x live-py-mode
;;   If that doesn't work, put the following in your Emacs configuration file:

(use-package live-py-mode
  :defer t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Frontend to python package manager pip
;;   https://github.com/brotzeitmacher/pippel
;;
;;   M-x pippel-list-packages

(use-package pippel
  :defer t
  )

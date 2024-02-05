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

;; python-mode MELPA
;;   https://gitlab.com/python-mode-devs/python-mode

;; python-mode remap C-<delete> C-<backspace>
;; (setq python-mode-map
;;       (let ((map (make-sparse-keymap)))
;;         (define-key map [(control backspace)] 'py-hungry-delete-backwards)
;;         (define-key map [(control c) (delete)] 'py-hungry-delete-forward)

;; Fixme: sometimes python-mode is not loaded
;; check Python menu
(require 'python-mode)

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

(use-package emacs
  :bind (:map python-mode-map
	      ("C-<delete>" . py-kill-word)
              ("C-<backspace>" . py-backward-kill-word)
	 :map python-ts-mode-map
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
;; Company JEDI
;;   https://github.com/syohex/emacs-company-jedi

(add-hook 'python-mode-hook (lambda ()
			      (add-to-list 'company-backends 'company-jedi)
			      ;; https://github.com/syl20bnr/spacemacs/issues/15137
			      ;; don't work ??? see lsp config
			      ;; (setq flycheck-checker 'python-flake8)
	  ))

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

(use-package ein)
;; (use-package ein-loaddefs)
(use-package ein-notebook)
;; (use-package ein-subpackages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Live Coding in Python
;;   http://donkirkby.github.io/live-py-plugin/
;;
;;   To use it, open a Python file and run M-x live-py-mode
;;   If that doesn't work, put the following in your Emacs configuration file:

(use-package live-py-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Frontend to python package manager pip
;;   https://github.com/brotzeitmacher/pippel
;;
;;   M-x pippel-list-packages

(use-package pippel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; importmagic.el
;;   https://github.com/anachronic/importmagic.el
;;
;;   The default behavior sets only one key binding: C-c C-l. It
;;   solves imports for every unresolved symbol in the buffer,
;;   prompting for one import at a time. If there are no imports found
;;   for a given symbol, importmagic will let you know at the end of
;;   the process.

; use to much cpu
; (add-hook 'python-mode-hook 'importmagic-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Pylookup
;

;; (setq pylookup-dir (concat local_emacs_site_lisp_path "pylookup"))
;; (add-to-list 'load-path pylookup-dir)
;;
;; ;; load pylookup when compile time
;; (eval-when-compile (use-package pylookup))
;;
;; ;; set executable file and db file
;; (setq pylookup-program (concat pylookup-dir "/pylookup.py"))
;; (setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))
;;
;; ;; set search option if you want
;; ;; (setq pylookup-search-options '("--insensitive" "0" "--desc" "0"))
;;
;; ;; to speedup, just load it on demand
;; (autoload 'pylookup-lookup "pylookup"
;;   "Lookup SEARCH-TERM in the Python HTML indexes." t)
;;
;; (autoload 'pylookup-update "pylookup"
;;   "Run pylookup-update and create the database at `pylookup-db-file'." t)
;;
;; (global-set-key "\C-ch" 'pylookup-lookup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Ropemacs
;

;; ; (use-package pymacs)
;; ; (pymacs-load "ropemacs" "rope-")
;;
;; (defun load-ropemacs ()
;;   "Load pymacs and ropemacs"
;;   (interactive)
;;   (use-package pymacs)
;;   (pymacs-load "ropemacs" "rope-")
;;   ;; Automatically save project python buffers before refactorings
;;   (setq ropemacs-confirm-saving 'nil)
;;   ; (setq ropemacs-enable-autoimport t)
;;   )
;; ; (global-set-key "\C-xpl" 'load-ropemacs)

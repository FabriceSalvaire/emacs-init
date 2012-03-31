;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Autoconf
;

(autoload 'autoconf-mode "autoconf-mode"
  "Major mode for editing autoconf files." t)
(setq auto-mode-alist
      (cons '("\\.ac\\'\\|configure\\.in\\'" . autoconf-mode)
            auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; C
;

(add-to-list 'auto-mode-alist '("\\.txx\\'" . c-mode))

(add-hook 'c-mode-common-hook
          '(lambda () (c-set-style "gnu")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; CSS
;

(autoload 'css-mode "css-mode")
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Cuda
;

(autoload 'cuda-mode "cuda-mode")
(add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Doxygen
;

; (require 'doxymacs)
; (add-hook 'c-mode-common-hook 'doxymacs-mode)
; (defun my-doxymacs-font-lock-hook ()
;   (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;       (doxymacs-font-lock)))
; (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; GLSL
;

(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Graphviz Dot
;

(load "graphviz-dot-mode.el" t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; GTags
;

; (autoload 'gtags-mode "gtags" "" t)
; (setq c-mode-hook
;       '(lambda ()
;          (gtags-mode 1)
;          ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Perl
;

; (add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
; (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
; (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
; (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
; (defalias 'perl-mode 'cperl-mode)
; (setq cperl-invalid-face (quote off)) ; pour ____
; (setq cperl-electric-keywords t)
; (setq cperl-hairy t)
; (setq cperl-auto-newline t)
; (setq cperl-highlight-variables-indiscriminately t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; PHP
;

; (require 'php-mode)
; (add-hook 'php-mode-user-hook 'turn-on-font-lock)
; (add-hook 'php-mode-user-hook
;         '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Python
;

; (setq interpreter-mode-alist
;       (cons '("python" . python-mode)
;             interpreter-mode-alist))

; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

; (autoload 'python-mode "python-mode" "Python Mode." t)   
; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))   
; (add-to-list 'interpreter-mode-alist '("python" . python-mode))   
; 
; (global-set-key [M-right] 'py-forward-into-nomenclature)
; (global-set-key [M-left] 'py-backward-into-nomenclature)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; VB
;

(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\)$" .
                                visual-basic-mode)) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Yaml
;

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Unlike python-mode, this mode follows the Emacs convention of not
;; binding the ENTER key to `newline-and-indent'.  To get this
;; behavior, add the key definition to `yaml-mode-hook':
;;
;;    (add-hook 'yaml-mode-hook
;;     '(lambda ()
;;        (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Anything
;

(add-to-list 'load-path (concat local_emacs_site_lisp_path "anything"))

;; (require 'anything)
;; (require 'anything-ipython)
;; (when (require 'anything-show-completion nil t)
;;   (use-anything-show-completion 'anything-ipython-complete
;; 				'(length initial-pattern)))

; (require 'anything-config)
; (require 'anything-match-plugin)
; (global-set-key "\C-ca" 'anything)
; (global-set-key "\C-ce" 'anything-for-files)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Pylookup
;

(setq pylookup-dir (concat local_emacs_site_lisp_path "pylookup"))
(add-to-list 'load-path pylookup-dir)

;; load pylookup when compile time
(eval-when-compile (require 'pylookup))

;; set executable file and db file
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; set search option if you want
;; (setq pylookup-search-options '("--insensitive" "0" "--desc" "0"))

;; to speedup, just load it on demand
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)

(autoload 'pylookup-update "pylookup" 
  "Run pylookup-update and create the database at `pylookup-db-file'." t)

(global-set-key "\C-ch" 'pylookup-lookup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Yasnippet
;

; yas/insert-snippet: C-c & C-s

(add-to-list 'load-path (concat local_emacs_site_lisp_path "yasnippet"))

(require 'yasnippet)
(setq yas/snippet-dirs (concat local_emacs_d_path "snippets"))
(yas/global-mode 1)
;; (load "snippet-bundel.el" t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Auto-Complete
;

(require 'auto-complete)
(global-auto-complete-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Ropemacs
;

; (require 'pymacs)
; (pymacs-load "ropemacs" "rope-")

(defun load-ropemacs ()
  "Load pymacs and ropemacs"
  (interactive)
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-")
  ;; Automatically save project python buffers before refactorings
  (setq ropemacs-confirm-saving 'nil)
  ; (setq ropemacs-enable-autoimport t)
  )
; (global-set-key "\C-xpl" 'load-ropemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Uniquify
;

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers (or Gnus mail buffers) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Line Number
;

; (require 'linum)   
; (global-linum-mode 1)  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Column Marker
;

(require 'column-marker)

;; Highlight column 100 in python mode.
(add-hook 'python-mode-hook '(lambda () (interactive) (column-marker-1 100)))

;; Use `C-c m' interactively to highlight with face `column-marker-1'.
;; (global-set-key [?\C-c ?m] 'column-marker-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Highlight Indentation
;

(require 'highlight-indentation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; End
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

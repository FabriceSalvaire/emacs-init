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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; pyvenv
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Company JEDI
;;   https://github.com/syohex/emacs-company-jedi

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Pylookup
;

;;;; (setq pylookup-dir (concat local_emacs_site_lisp_path "pylookup"))
;;;; (add-to-list 'load-path pylookup-dir)
;;;;
;;;; ;; load pylookup when compile time
;;;; (eval-when-compile (require 'pylookup))
;;;;
;;;; ;; set executable file and db file
;;;; (setq pylookup-program (concat pylookup-dir "/pylookup.py"))
;;;; (setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))
;;;;
;;;; ;; set search option if you want
;;;; ;; (setq pylookup-search-options '("--insensitive" "0" "--desc" "0"))
;;;;
;;;; ;; to speedup, just load it on demand
;;;; (autoload 'pylookup-lookup "pylookup"
;;;;   "Lookup SEARCH-TERM in the Python HTML indexes." t)
;;;;
;;;; (autoload 'pylookup-update "pylookup"
;;;;   "Run pylookup-update and create the database at `pylookup-db-file'." t)
;;;;
;;;; (global-set-key "\C-ch" 'pylookup-lookup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Ropemacs
;

;;;; ; (require 'pymacs)
;;;; ; (pymacs-load "ropemacs" "rope-")
;;;;
;;;; (defun load-ropemacs ()
;;;;   "Load pymacs and ropemacs"
;;;;   (interactive)
;;;;   (require 'pymacs)
;;;;   (pymacs-load "ropemacs" "rope-")
;;;;   ;; Automatically save project python buffers before refactorings
;;;;   (setq ropemacs-confirm-saving 'nil)
;;;;   ; (setq ropemacs-enable-autoimport t)
;;;;   )
;;;; ; (global-set-key "\C-xpl" 'load-ropemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; End
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

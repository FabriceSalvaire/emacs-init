;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Modes for Programming Languages
;; Sorted by names
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Autoconf
;;

(use-package autoconf-mode
  :mode ("\\.ac\\'" "configure\\.in\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CMake
;;

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; https://github.com/Lindydancer/cmake-font-lock
(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Cuda
;;

(use-package cuda-mode
  :mode "\\.cu\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; GLSL
;;

(use-package glsl-mode
  :mode ("\\.vert\\'" "\\.frag\\'" "\\.glsl\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Go
;;

(use-package go-mode
  :mode "\\.go\\'"
  :config
  (add-hook 'go-mode-hook
	    (lambda ()
	      (setq-default)
	      (setq tab-width 4)
	      (setq standard-indent 4)
	      (setq indent-tabs-mode nil)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Graphviz Dot
;

;;(use-package graphviz-dot-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Java
;   -> C

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Kotlin
;

(use-package kotlin-mode
  :mode "\\.kt\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; QMake
;

(use-package qmake-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; QML
;

(use-package qml-mode
  :mode "\\.qml\\'"
  :preface
  (defun my-qml-hook ()
    (progn
      (setq-default js-indent-level 4)
      (setq indent-tabs-mode nil)
      ))
  :config
  (add-hook 'qml-mode-hook 'my-qml-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ReST
;;

(use-package rst
  :mode "\\.rst\\'"
  :config
  (add-hook 'rest-mode-hook (lambda ()
                              (setq indent-tabs-mode nil)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TeX / LaTeX
;;   AUC-TeX

(use-package auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :hook (LaTeX-mode turn-on-reftex) ; with AUCTeX LaTeX mode
  :hook (latex-mode turn-on-reftex) ; with Emacs latex mode

  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)

  ;; (if window-system
  ;;     (use-package font-latex))
  )

;; (load "preview-latex.el" nil t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Yaml
;

(use-package yaml-mode
  :mode "\\.yml\\'")

;; Unlike python-mode, this mode follows the Emacs convention of not
;; binding the ENTER key to `newline-and-indent'.  To get this
;; behavior, add the key definition to `yaml-mode-hook':
;;
;;    (add-hook 'yaml-mode-hook
;;     '(lambda ()
;;        (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Modelica
;

;; (add-to-list 'load-path (concat local_emacs_site_lisp_path "modelica/"))
;; (add-to-list 'load-path (concat local_emacs_site_lisp_path "modelica-mode"))

;; (autoload 'modelica-mode "modelica-mode" "Modelica Editing Mode" t)
;; (setq auto-mode-alist (cons '("\.mo$" . modelica-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; C#
;

;; (autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
;; (setq auto-mode-alist
;;       (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Doxygen
;

; (use-package doxymacs)
; (add-hook 'c-mode-common-hook 'doxymacs-mode)
; (defun my-doxymacs-font-lock-hook ()
;   (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;       (doxymacs-font-lock)))
; (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Graddle
;

;; (use-package gradle-mode)
;; (gradle-mode 1)

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

; (use-package php-mode)
; (add-hook 'php-mode-user-hook 'turn-on-font-lock)
; (add-hook 'php-mode-user-hook
;         '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; VB
;

;; (autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
;; (setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\)$" .
;;                                 visual-basic-mode)) auto-mode-alist))

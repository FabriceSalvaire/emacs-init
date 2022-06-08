;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Modes for Programming Languages
;; Sorted by names
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Autoconf
;

(autoload 'autoconf-mode "autoconf-mode"
  "Major mode for editing autoconf files." t)
(setq auto-mode-alist
      (cons '("\\.ac\\'\\|configure\\.in\\'" . autoconf-mode)
            auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; CMake
;

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Cuda
;

(autoload 'cuda-mode "cuda-mode"
  :mode "\\.cu\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; GLSL
;

(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Go
;

(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(add-hook 'go-mode-hook
  (lambda ()
    (setq-default)
    (setq tab-width 4)
    (setq standard-indent 4)
    (setq indent-tabs-mode nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Graphviz Dot
;

(load "graphviz-dot-mode.el" t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Java
;

;; (defun my-java-hook ()
;;     (progn
;;       (setq-default c-basic-offset 4
;; 		    indent-tabs-mode nil)
;;       ))
;; (add-hook 'java-mode-hook 'my-java-hook)

(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2
                                  indent-tabs-mode nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Kotlin
;

;; (use-package kotlin-mode)
;; (add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-mode))

(autoload 'kotlin-mode "kotlin-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Modelica
;

(add-to-list 'load-path (concat local_emacs_site_lisp_path "modelica/"))
(add-to-list 'load-path (concat local_emacs_site_lisp_path "modelica-mode"))

(autoload 'modelica-mode "modelica-mode" "Modelica Editing Mode" t)
(setq auto-mode-alist (cons '("\.mo$" . modelica-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; QMake
;

(use-package qmake-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; QML
;

(autoload 'qml-mode "qml-mode" "Qml Editing Mode" t)
(setq auto-mode-alist (cons '("\.qml$" . qml-mode) auto-mode-alist))

(defun my-qml-hook ()
    (progn
      (setq-default js-indent-level 4)
      (setq indent-tabs-mode nil)
      ))
(add-hook 'qml-mode-hook 'my-qml-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ReST
;;

(add-hook 'rest-mode-hook (lambda ()
                            (setq indent-tabs-mode nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TeX / LaTeX
;;

; AUC-TeX

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

; (if window-system
;     (use-package font-latex))

(add-hook 'LaTeX-mode-hook 'turn-on-reftex) ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode

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

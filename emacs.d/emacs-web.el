;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CSS
;;

(autoload 'css-mode "css-mode")
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; django-mode
;;   https://github.com/myfreeweb/django-mode

;; (add-to-list 'load-path (concat local_emacs_site_lisp_path "django-mode/"))
;; (require 'django-html-mode)
;; (require 'django-mode)
;; (yas/load-directory (concat local_emacs_site_lisp_path "django-mode/snippets/"))
;; (add-to-list 'auto-mode-alist '("\\.djhtml$" . django-html-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; python-django-mode
;;  https://github.com/fgallina/python-django.el
;;

; (require 'python-django) ; flycheck issue

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SCSS / SASS
;;

(require 'sass-mode)

; (setq exec-path (cons (expand-file-name "~/.gem/ruby/1.8/bin") exec-path))
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; web-mode
;;
;; http://web-mode.org

;; (require 'web-mode) ; require django-mode
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; (defun my-web-mode-hook () "Hooks for Web mode."
;;   (setq web-mode-enable-engine-detection t)
;;   (setq web-mode-markup-indent-offset 4)
;;   (setq web-mode-css-indent-offset 4)
;;   (setq web-mode-code-indent-offset 4)
;;   )
;; (add-hook 'web-mode-hook 'my-web-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Typoscript
;;

;; https://github.com/ksjogo/typoscript-mode
;; https://github.com/emacsmirror/ts-mode

(setq auto-mode-alist (cons '("\\.ts$" . typoscript-mode) auto-mode-alist))
(autoload 'typoscript-mode "typoscript-mode" "TypoScript file editing mode." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; JSX
;;

;; https://github.com/felipeochoa/rjsx-mode/

(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))

(add-hook 'jsx-mode-hook (lambda ()
                           (setq c-basic-offset 4
                                 indent-tabs-mode nil)))

(add-hook 'js2-mode-hook (lambda ()
                           (setq c-basic-offset 4
                                 indent-tabs-mode nil)))

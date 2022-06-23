;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CSS
;;

(use-package css-mode
  :mode "\\.css\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SCSS / SASS
;;

(use-package sass-mode
  :mode "\\.scss\\'"
  )

; (setq exec-path (cons (expand-file-name "~/.gem/ruby/1.8/bin") exec-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; JS
;;
;;  https://github.com/mooz/js2-mode
;; http://elpa.gnu.org/packages/js2-mode.html
;;

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (add-hook 'js2-mode-hook (lambda () (setq c-basic-offset 4
					    indent-tabs-mode nil)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; JSX
;;
;;   https://github.com/felipeochoa/rjsx-mode/
;;

;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
;; (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))

(use-package rjsx-mode
  :config
  (add-hook 'rjsx-mode-hook (lambda () (setq c-basic-offset 4
					     indent-tabs-mode nil)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; web-mode
;;
;; http://web-mode.org

(use-package web-mode
  ;; require django-mode
  :mode "\\.html?\\'"
  :config
  (add-hook 'web-mode-hook (lambda () (setq web-mode-enable-engine-detection t
					    web-mode-markup-indent-offset 4
					    web-mode-css-indent-offset 4
					    web-mode-code-indent-offset 4)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Typoscript
;;   https://github.com/ksjogo/typoscript-mode
;;   https://github.com/emacsmirror/ts-mode

(use-package typoscript
  :mode "\\.ts\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; django-mode
;;   https://github.com/myfreeweb/django-mode

;; (add-to-list 'load-path (concat local_emacs_site_lisp_path "django-mode/"))
;; (use-package django-html-mode)
;; (use-package django-mode)
;; (yas/load-directory (concat local_emacs_site_lisp_path "django-mode/snippets/"))
;; (add-to-list 'auto-mode-alist '("\\.djhtml$" . django-html-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; python-django-mode
;;  https://github.com/fgallina/python-django.el
;;

; (use-package python-django) ; flycheck issue

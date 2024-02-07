;;; .el --- ... -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; web-mode — major-mode for editing web templates
;;   http://web-mode.org
;;   smart indentation (according to the context : HTML, code, JavaScript or CSS)
;;   compatibility with many template engines : php, django, jinja, jsx
;;      php, jsp, gsp (grails), asp / asp.net ajax (atlas), django / twig / jinja(2) / erlydtl (zotonic) / selmer, erb, ejs, freemarker, velocity, cheetah, smarty, ctemplate / mustache / hapax / handlebars / meteor / blaze / ember.js / velvet, blade (laravel), knockoutjs, go template (revel), razor/play, dust, closure (soy), underscore.js, template-toolkit, liquid (jekyll), angular.js, web2py, mako (pylons), reactjs (jsx), mojolicious, elixir (erlang), thymeleaf, cl-emb, heist, archibus, xoops, hero, spip, svelte, elixir

(use-package web-mode
  :ensure t
  ;; require django-mode
  :mode "\\.html?\\'"
  :init
  (add-hook 'web-mode-hook (lambda () (setq web-mode-enable-engine-detection t
					    web-mode-markup-indent-offset 4
					    web-mode-css-indent-offset 4
					    web-mode-code-indent-offset 4)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CSS
;;

(use-package css-mode
  :ensure t
  :mode "\\.css\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SCSS / SASS
;;

(use-package sass-mode
  :ensure t
  :mode "\\.scss\\'"
  ;; (setq exec-path (cons (expand-file-name "~/.gem/ruby/1.8/bin") exec-path))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; JS
;;   https://github.com/mooz/js2-mode
;;   http://elpa.gnu.org/packages/js2-mode.html
;;

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  ;; :hook (prog-mode text-mode)
  :init
  (add-hook 'js2-mode-hook (lambda () (setq c-basic-offset 4
					    indent-tabs-mode nil)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Typescript
;;

;; (use-package typescript
(use-package typescript-ts-mode
  :ensure t
  :mode "\\.ts\\'"
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
  :ensure t
  :init
  (add-hook 'rjsx-mode-hook (lambda () (setq c-basic-offset 4
					     indent-tabs-mode nil)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Astro
;;   https://github.com/Sorixelle/astro-ts-mode
;;   https://github.com/virchau13/tree-sitter-astro

(use-package astro-ts-mode
  :ensure t
  :config
  (setq astro-ts-mode-indent-offset 4)
  :init
  (add-hook 'astro-ts-mode-hook (lambda () (setq indent-tabs-mode nil)))
  )

;; (define-derived-mode astro-mode web-mode "astro")
;; (setq auto-mode-alist
;;       (append '((".*\\.astro\\'" . astro-mode))
;;               auto-mode-alist))


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

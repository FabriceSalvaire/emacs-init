;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Projectile
;;   https://github.com/bbatsov/projectile
;;   https://docs.projectile.mx/projectile/index.html
;;
;;  dnf install fd-find the_silver_searcher ripgrep
;;    fd — find entries in the filesystem
;;    ag — The Silver Searcher
;;    rg — recursively search the current directory for lines matching a pattern

;; (add-to-list 'package-pinned-packages '(projectile . "melpa-stable") t)

(use-package projectile
  ;;s;; :ensure t

  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))

  :init
  (projectile-mode +1)

  ;; https://github.com/anshulverma/projectile-speedbar
  ;; versus treemacs-projectile
  ;; (use-package projectile-speedbar)

  :config
  (setq
   ;; Recursive discovery is configured by specifying the search depth in a cons cell
   projectile-project-search-path '(("~/__projects__/" . 1))

   ;; projectile-sort-order 'default
   projectile-sort-order 'recently-active

   projectile-completion-system 'ivy
   )

  ;; https://docs.projectile.mx/projectile/projects.html
  (projectile-register-project-type 'CMakeGit
                                    '("CMakeLists.txt" ".git")
                                    :project-file "CMakeLists.txt"
                                    :compile ""
                                    :test ""
                                    :run ""
                                    :test-suffix ""
                                    )
  )

;; https://github.com/ericdanan/counsel-projectile for Ivy integration
(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode 1))

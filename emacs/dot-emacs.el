;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                  ;
;;                                              GNU-Emacs                                           ;
;;                                                                                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; M-x load-file ~./emacs
;;
;; C-x C-f /path/to/... .el RET
;; M-x byte-compile-file RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Path Settings
;;

(setq local_path_prefix "/home/common/emacs/" ; this is the only absolut path for Emacs files
      local_emacs_site_lisp_path (concat local_path_prefix "emacs-site-lisp/")
      local_emacs_d_path (concat local_path_prefix "emacs.d/")
      )

(add-to-list 'load-path local_emacs_d_path)
(add-to-list 'load-path local_emacs_site_lisp_path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "packages.el" t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load sub-config files
;;

(load "variables.el" t t)
(load "user-functions.el" t t)

(load "frame.el" t t)
(load "font.el" t t)
(load "behaviour.el" t t)
(load "encoding.el" t t)
(load "treemacs-settings.el" t t)
(load "completion.el" t t) ; buffer switch
(load "edition-settings.el" t t)

(load "modes.el" t t)
(load "sysadmin.el" t t)

(load "programming.el" t t)
(load "mode-languages.el" t t)
(load "mode-c.el" t t)
(load "mode-python.el" t t)
(load "mode-web.el" t t)

(load "spelling.el" t t)
(load "flycheck-settings.el" t t)

;; must be after
(load "keys.el" t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom Settings
;;

;; (setq custom-file (concat local_path_prefix "gnu-emacs-custom.el"))
;; (load custom-file t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load post-config files
;;

(load "server-settings.el" t t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(package-selected-packages
   '(multiple-cursors treesit-auto eglot corfu zzz-to-char python-mode all-the-icons-ivy anything apache-mode auctex-lua auto-complete auto-complete-c-headers cff clang-format clean-aindent-mode cmake-font-lock cmake-ide cmake-mode cmake-project column-marker company-anaconda company-auctex company-box company-c-headers company-irony company-irony-c-headers company-jedi company-math company-qml company-shell cpputils-cmake csharp-mode cuda-mode dap-mode django-mode docker-compose-mode dockerfile-mode doom-modeline dotenv-mode flycheck-color-mode-line flycheck-grammalecte flycheck-irony flycheck-kotlin flycheck-pos-tip flycheck-pycheckers flycheck-tip flymake-shell flyspell-correct-ivy flyspell-lazy glsl-mode go-mode google-translate gradle-mode graphql-mode graphviz-dot-mode haskell-mode highlight-indentation icicles icomplete+ importmagic indent-guide irony-eldoc jinja2-mode json-mode kotlin-mode language-detection languagetool live-py-mode magit markdown-mode markdown-mode+ markdown-preview-mode markdown-toc modern-cpp-font-lock php-mode pippel powerline projectile projectile-ripgrep projectile-speedbar protobuf-mode pyimport pylint python-django pyvenv qml-mode react-snippets rjsx-mode sass-mode scss-mode smartparens spice-mode string-inflection systemd tern treemacs-all-the-icons treemacs-icons-dired treemacs-magit treemacs-perspective treemacs-projectile treemacs-tab-bar typoscript-mode vue-mode which-key))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Noto Sans Mono" :foundry "GOOG" :slant normal :weight normal :height 143 :width normal)))))

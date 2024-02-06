;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Top Emacs Configuration
;;

;; M-x load-file ~./emacs
;;
;; C-x C-f /path/to/... .el RET
;; M-x byte-compile-file RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Path Settings
;;

(defconst local_path_prefix "/home/common/emacs/") ; this is the only absolut path for Emacs files
(defconst local_emacs_site_lisp_path (concat local_path_prefix "emacs-site-lisp/"))
(defconst local_emacs_d_path (concat local_path_prefix "emacs.d/"))

(add-to-list 'load-path local_emacs_d_path)
(add-to-list 'load-path local_emacs_site_lisp_path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Increase GC
;;   The default is 800 kilobytes.
;;   reseted at the end
;;   see also lsp config !
(setq gc-cons-threshold (* 100 1024 1024)) ; 100 MB

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load sub-config files
;;

(dolist (_
	 '(
	   "packages"
	   "frame"
	   "core"

	   "user-functions"

	   "file-manager"
	   "completion" ; buffer switch
	   "edition"
	   "speller"

	   "sysadmin"

	   "code-completion"
	   "lang/lang"
	   "lang/c-java"
	   "lang/python"
	   "lang/web"
	   "checker"

	   ;; must be after
	   "keys"
	   )
	 )
  ;; (load FILE &optional NOERROR NOMESSAGE NOSUFFIX MUST-SUFFIX)
  (load (concat _ ".el") t t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom Settings
;;

;; (setq custom-file (concat local_path_prefix "gnu-emacs-custom.el"))
;; (load custom-file t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Server
;;

;; Starts server for (among others) emacsclient
(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1024 1024))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dap-cpptools all-the-icons-ivy anything apache-mode auctex-lua auto-complete auto-complete-c-headers cff clang-format clean-aindent-mode cmake-font-lock cmake-ide cmake-mode cmake-project column-marker company-anaconda company-auctex company-box company-c-headers company-irony company-irony-c-headers company-jedi company-math company-qml company-shell cpputils-cmake csharp-mode cuda-mode dap-mode django-mode docker-compose-mode dockerfile-mode doom-modeline dotenv-mode flycheck-color-mode-line flycheck-grammalecte flycheck-irony flycheck-kotlin flycheck-pos-tip flycheck-pycheckers flycheck-tip flymake-shell flyspell-correct-ivy flyspell-lazy glsl-mode go-mode google-translate gradle-mode graphql-mode graphviz-dot-mode haskell-mode highlight-indentation icicles icomplete+ importmagic indent-guide irony-eldoc jinja2-mode json-mode kotlin-mode language-detection languagetool live-py-mode magit markdown-mode markdown-mode+ markdown-preview-mode markdown-toc modern-cpp-font-lock php-mode pippel powerline projectile projectile-ripgrep projectile-speedbar protobuf-mode pyimport pylint python-django pyvenv qml-mode react-snippets rjsx-mode sass-mode scss-mode smartparens spice-mode string-inflection systemd tern treemacs-all-the-icons treemacs-icons-dired treemacs-magit treemacs-perspective treemacs-projectile treemacs-tab-bar typoscript-mode vue-mode which-key)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Noto Sans Mono" :foundry "GOOG" :slant normal :weight normal :height 143 :width normal)))))

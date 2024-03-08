;;; .el --- ... -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Google Translate
;;   https://github.com/atykhonov/google-translate

(use-package google-translate
  ;;s;; :ensure t
  :defer t
  :custom
  (google-translate-translation-directions-alist '(("en" . "fr") ("fr" . "en")))
  :config
  ;; (use-package google-translate-default-ui)
  (use-package google-translate-smooth-ui)
  )

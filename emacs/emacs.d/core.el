;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Core Settings
;;

(use-package emacs
  :config

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Emacs Reset
  ;;

  ;; Override Emacs function risky-local-variable-p
  ;;   allow remembering risky variables
  ;;   for spam asking y/n for some files...
  (defun risky-local-variable-p (sym &optional _ignored) nil)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Compilation
  ;;

  ;; Whether to report warnings and errors from asynchronous native compilation
  (setq native-comp-async-report-warnings-errors nil)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Default Encoding
  ;;
  (set-language-environment 'utf-8)
  (setq-default buffer-file-coding-system 'utf-8-unix)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Recent — minor mode that builds a list of recently opened files
  ;;  https://www.emacswiki.org/emacs/RecentFiles
  ;; Enable menu for recently opened files
  (recentf-mode t)
  ;; (setq recentf-max-menu-items 25)
  ;; (setq recentf-max-saved-items 25)
  ;; Periodically saving the list of files
  ;; (run-at-time nil (* 5 60) 'recentf-save-list)
  ;; (global-set-key "\C-x\ \C-r" 'recentf-open-files)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Buffer
  ;;

  ;; Uniquify — overrides Emacs default mechanism for making buffer names unique
  ;;  https://www.emacswiki.org/emacs/uniquify
  (setq uniquify-buffer-name-style 'reverse
	uniquify-separator "/"
	uniquify-after-kill-buffer-p t ; rename after killing uniquified
	uniquify-ignore-buffers-re "^\\*" ; don't muck with special buffers (or Gnus mail buffers)
	)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Recent — minor mode that builds a list of recently opened files
;;  https://www.emacswiki.org/emacs/RecentFiles

; Menu for recently opened files
(recentf-mode t)
;; (setq recentf-max-menu-items 25)
;; (setq recentf-max-saved-items 25)
;; (global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Periodically saving the list of files
;; (run-at-time nil (* 5 60) 'recentf-save-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Uniquify — overrides Emacs default mechanism for making buffer names unique
;;  https://www.emacswiki.org/emacs/uniquify

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers (or Gnus mail buffers)

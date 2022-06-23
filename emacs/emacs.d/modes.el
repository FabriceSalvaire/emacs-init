;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Dired
;;

;; (use-package dired
;;   :config
;;  )

(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            ;; Set dired-x global variables here.
            ))

;; Fixme: symbol value as variable is void
(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.
            ;; dired-omit-toggle M-o
            ;; (setq dired-omit-files-p t)
            (setq dired-omit-files
                  ;; do not wish to see `dot' files (files starting with a `.')
		  ;;  '(dired-omit-files "^\\.$")
                  (concat dired-omit-files "\\|^\\..+$"))
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Info
;

; (setq Info-directory-list Info-default-directory-list)
; (add-to-list 'Info-directory-list "/media/home/salvaire/local_stow/info")

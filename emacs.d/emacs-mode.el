;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; iswitchb
;

(require 'iswitchb) 
; (iswitchb-default-keybindings) 
(iswitchb-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; dired
;

(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            ;; Set dired-x global variables here.
            ))

(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.
            ;; dired-omit-toggle M-o
            ; (setq dired-omit-files-p t)
            (setq dired-omit-files
                  ; do not wish to see `dot' files (files starting with a `.')
                  (concat dired-omit-files "\\|^\\..+$"))
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Mutt
;

(autoload 'muttrc-mode "muttrc-mode.el" "Major mode to edit muttrc files" t)
(setq auto-mode-alist
      (append '(("muttrc\\'" . muttrc-mode))
              auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Info
;

; (setq Info-directory-list Info-default-directory-list)
; (add-to-list 'Info-directory-list "/media/home/salvaire/local_stow/info")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; End
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

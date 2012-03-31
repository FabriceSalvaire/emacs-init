;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; FlyMake
;

; (add-to-list 'load-path (concat local_emacs_site_lisp_path "flymake-python"))

;; Configure flymake for python
(setq pylint (concat local_emacs_site_lisp_path "pylint/epylint"))
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list (expand-file-name pylint "") (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" "Python" flymake-pylint-init)))

;; Set as a minor mode for python
(add-hook 'python-mode-hook '(lambda () (flymake-mode 1)))

;; Configure to wait a bit longer after edits before starting
(setq-default flymake-no-changes-timeout '3)

;; Keymaps to navigate to the errors
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-cn" 'flymake-goto-next-error)))
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-cp" 'flymake-goto-prev-error)))

;; To avoid having to mouse hover for the error message, these functions make flymake error messages
;; appear in the minibuffer
(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the message in the minibuffer"
  (require 'cl)
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
    	  (let ((err (car (second elem))))
    	    (message "%s" (flymake-ler-text err)))))))

(add-hook 'post-command-hook 'show-fly-err-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (when (load "flymake" t)
;   (defun flymake-pylint-init (&optional trigger-type)
;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;                        'flymake-create-temp-with-folder-structure))
; 	   (local-file (file-relative-name
; 			temp-file
; 			(file-name-directory buffer-file-name)))
; 	   (options (when trigger-type (list "--trigger-type" trigger-type))))
;       (list "/home/etc/users/emacs-site-lisp/flymake-python/pyflymake.py"
; 	    (append options (list local-file)))))
;   (add-to-list 'flymake-allowed-file-name-masks
; 	       '("\\.py\\'" flymake-pylint-init)))
; 
; (add-hook 'find-file-hook 'flymake-find-file-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; End
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
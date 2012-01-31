;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; FlyMake
;

(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))
  
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

(add-hook 'python-mode-hook 'flymake-mode)

;;; (when (load "flymake" t)
;;;   (defun flymake-pylint-init (&optional trigger-type)
;;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;;                        'flymake-create-temp-with-folder-structure))
;;; 	   (local-file (file-relative-name
;;; 			temp-file
;;; 			(file-name-directory buffer-file-name)))
;;; 	   (options (when trigger-type (list "--trigger-type" trigger-type))))
;;;       (list "/home/etc/users/emacs-site-lisp/flymake-python/pyflymake.py"
;;; 	    (append options (list local-file)))))
;;; 
;;;   (add-to-list 'flymake-allowed-file-name-masks
;;; 	       '("\\.py\\'" flymake-pylint-init)))
;;; 
;;; (add-hook 'find-file-hook 'flymake-find-file-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; End
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
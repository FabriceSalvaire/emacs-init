;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Insert CopyRight
;

(defun insert-copyright ()
  (interactive)
  (save-excursion
    (let ((file (file-name-nondirectory buffer-file-name))
	  (copyright_file))
      (progn
	(if (string-equal (file-name-extension file) "h")
	    ((let* ((name (concat "__" (upcase (file-name-sans-extension file)) "_H__"))
		    (text (concat "\n#ifndef " name "\n"
				  "#define "   name "\n\n\n\n"
				  "#endif /* " name " */")))
	       (goto-char (point-min))
	       (insert text))))
	(goto-char (point-min))
	(cond ((string-match "\\.py\\'" file)
	       (setq copyright_file "copyright-header.py"))
	      ((string-match "\\.\\(h\\|c\\|hpp\\|cpp\\|cc\\)\\'" file)
	       (setq copyright_file "copyright-header.c")))
	(insert-file-contents (concat local_emacs_d_path copyright_file))
	))))
(global-set-key [f3] 'insert-copyright)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Insert Frame
;

;; file-name-extension

(defun insert-audit ()
  (interactive)
  (save-excursion
    (let ((file (file-name-nondirectory buffer-file-name))
	  (audit_file))
      (progn
	(cond ((string-match "\\.py\\'" file)
	       (setq audit_file "audit-header.py"))
	      ((string-match "\\.\\(h\\|c\\|hpp\\|cpp\\|cc\\)\\'" file)
	       (setq audit_file "audit-header.c")))
	(insert-file-contents (concat local_emacs_d_path audit_file))
	))))
(global-set-key [f4] 'insert-audit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Insert Audit Date
;

(defun insert-audit-date ()
  (interactive)
  (insert (format-time-string "# - %d/%m/%Y")))
(global-set-key [f5] 'insert-audit-date)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(defun insert-inf-sup ()
;  (interactive)
;  (insert "<")
;  (save-excursion
;    (insert ">")))
;(global-set-key "\M-<" 'insert-inf-sup)

;(defun insert-slash ()
;  (interactive)
;  (insert "/")
;  (save-excursion
;    (insert "/")))
;(global-set-key "\M-/" 'insert-slash)

; (defun insert-braces ()
;   (interactive)
;   (insert "{")
;   (save-excursion
;     (insert "}")))
; (global-set-key "\M-{" 'insert-braces)
;
; (defun insert-brakets ()
;   (interactive)
;   (insert "[")
;   (save-excursion
;     (insert "]")))
; (global-set-key "\M-[" 'insert-brakets)
;
; (defun insert-backticks ()
;   (interactive)
;   (insert "`")
;   (save-excursion
;     (insert "`")))
; (global-set-key "\M-`" 'insert-backticks)
;
; (defun insert-simple-quotes ()
;   (interactive)
;   (insert "'")
;   (save-excursion
;     (insert "'")))
; (global-set-key "\M-'" 'insert-simple-quotes)
;
; (defun insert-double-quotes ()
;   (interactive)
;   (insert "\"")
;   (save-excursion
;     (insert "\"")))
; (global-set-key "\M-\"" 'insert-double-quotes)

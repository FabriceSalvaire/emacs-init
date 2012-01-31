;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 
;

(defun insert-copyright ()
  (interactive)
  (save-excursion
    (let ((file (file-name-nondirectory buffer-file-name)))
      (progn
	(if (string-match "\\([^\\.]*\\)\\.\\(h\\|c\\)" file)
	    (if (string-equal (match-string 2 file) "h")
		(let* ((name (concat "__" (upcase (match-string 1 file)) "_H__"))
		       (text (concat "\n#ifndef " name "\n"
				     "#define "   name "\n\n\n\n"
				     "#endif /* " name " */")))
		  (goto-char (point-min))
		  (insert text))))
	(goto-char (point-min))
	(if (string-match "\\.py$" file)
	    (insert-file-contents (concat local local_emacs_d_path "copyright-header.py")))
	(if (string-match "\\.\\(h\\|c\\|hpp\\|cpp\\|cc\\)$" file)
	    (insert-file-contents (concat "copyright-header.c")))
	))))
(global-set-key [f3] 'insert-copyright)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 
;

(defun insert-audit ()
  (interactive)
  (save-excursion
    (let ((file (file-name-nondirectory buffer-file-name)))
      (progn
	(if (string-match "\\.py$" file)
	    (insert-file-contents (concat local_emacs_d_path "audit-header.py")))
	(if (string-match "\\.\\(h\\|c\\|hpp\\|cpp\\|cc\\)$" file)
	    (insert-file-contents (concat local_emacs_d_path "audit-header.c")))
	))))
(global-set-key [f4] 'insert-audit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 
;

(defun insert-audit-date ()
  (interactive)
  (insert (format-time-string "# - %d/%m/%Y")))
(global-set-key [f5] 'insert-audit-date)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 
;

(defun insert-rule ()
  (interactive)
  (save-excursion
    (let ((file (file-name-nondirectory buffer-file-name)))
      (progn
	(if (string-match "\\.el$" file)
	    (insert (make-string 100 ?\;)))
	(if (string-match "\\.py$" file)
	    (insert (make-string 100 ?#)))
	(if (string-match "\\.\\(h\\|c\\|hpp\\|cpp\\|cc\\)$" file)
	    (insert (concat ?\\ (make-string 98 ?*)) ?\\))
	))))
(global-set-key [f6] 'insert-rule)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 
;

(defun insert-and ()
  (interactive)
  (insert "&&"))
(global-set-key "\M-&" 'insert-and)

(defun insert-or ()
  (interactive)
  (insert "||"))
(global-set-key "\M-|" 'insert-or)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; End
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

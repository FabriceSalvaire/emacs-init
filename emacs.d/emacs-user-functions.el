;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Python Mode Kill Word
;

(defun py-kill-word (arg)
  "Kill characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (kill-region (point) (progn (py-forward-into-nomenclature arg) (point))))
;(global-set-key [M-delete] 'kill-word)
(global-set-key [C-delete] 'py-kill-word)

(defun py-backward-kill-word (arg)
  "Kill characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (kill-region (point) (progn (py-backward-into-nomenclature arg) (point))))
;(global-set-key [M-backspace] 'backward-kill-word)
(global-set-key [C-backspace] 'py-backward-kill-word)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Insert Long Rule
;

(defun insert-long-rule ()
  (interactive)
  (save-excursion
    (let ((file (file-name-nondirectory buffer-file-name)))
      (progn
	(cond ((string-match "\\.el\\'" file)
	       (insert (make-string 100 ?\;)))
	      ((string-match "\\.\\(py\\|sh\\)\\'" file)
	       (insert (make-string 100 ?#)))
	      ((string-match "\\.\\(h\\|c\\|hpp\\|cpp\\|cc\\)\\'" file)
	       (insert (concat "/" (make-string 98 ?*) "/")))
	      ((string-match "\\.\\(sass\\|css\\)\\'" file)
	       (insert (concat "//" (make-string 98 ?*))))
	      ;; ((string-match "\\.\\(h\\|c\\|hpp\\|cpp\\|cc\\|sass\\|css\\)\\'" file)
	      ;;  (insert (concat "/" (make-string 98 ?*) "/")))
	      )
	))))
(global-set-key [f6] 'insert-long-rule)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Insert Long Rule
;

(defun insert-short-rule ()
  (interactive)
  (save-excursion
    (let ((file (file-name-nondirectory buffer-file-name)))
      (progn
	(if (string-match "\\.py\\'" file)
	    (insert (concat
		     (make-string 4 ? )
		     (make-string 46 ?#))))
	))))
(global-set-key [f7] 'insert-short-rule)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Indent
;

(defun custom-indent ()
  (interactive)
  (save-excursion
    (let ((from_to_list (list
			 '(" +//" "   //")
			 ;; '("else +//" "else   //")
			 ;; '("; +//" ";   //")
			 ;; '(", +//" ",   //")
			 ;; '(": +//" ":   //")
			 ;; '("{ +//" "{   //")
			 ;; '("} +//" "}   //")
			 )))
      (progn
	(untabify (point-min) (point-max))
	(dolist (from_to from_to_list)
	  (replace-regexp (car from_to) (cdr from_to) nil (point-min) (point-max))
	  )
	(indent-region (point-min) (point-max))
	))))
(global-set-key [f12] 'custom-indent)

(defun custom-doxygen ()
  (interactive)
  (save-excursion
    (let ((from_to_list (list
			 '("^\\( +\\)// " "\\1/// ")
			 '(";\\( +\\)// " ";\\1///< ")
			 '(",\\( +\\)// " ",\\1///< ")
			 )))
      (progn
	(dolist (from_to from_to_list)
	  (replace-regexp (car from_to) (cdr from_to) nil (point-min) (point-max))
	  )
	))))
(global-set-key [f11] 'custom-doxygen)

(defun c-comment-to-inline-style (start end)
  "Convert an ANSI-C style to C++ style"
  (interactive "r")
  (setq content (buffer-substring start end))
  (uncomment-region start end)
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-padding) " ")
  (set (make-local-variable 'comment-end) "")
  (comment-region start end)
  )

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

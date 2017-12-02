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
; Insert Long Rule
;

(defun insert-long-rule ()
  (interactive)
  (save-excursion
    (let ((file (file-name-nondirectory buffer-file-name)))
      (progn
	(message "mode is [%s]" major-mode)
	(cond ((equal major-mode 'emacs-lisp-mode)
	       (insert (make-string 100 ?\;)))
	      ((or (equal major-mode 'python-mode)
                   (equal major-mode 'shell-mode)
                   (equal major-mode 'cmake-mode))
	       (insert (make-string 100 ?#)))
	      ((or (equal major-mode 'c-mode)
                   (equal major-mode 'c++-mode))
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
; Insert Short Rule for Python
;

(defun insert-short-rule ()
  (interactive)
  (save-excursion
    (let ((file (file-name-nondirectory buffer-file-name)))
      (progn
	(if (equal major-mode 'python-mode)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun insert-and ()
  (interactive)
  (insert "&&"))
(global-set-key "\M-&" 'insert-and)

(defun insert-or ()
  (interactive)
  (insert "||"))
(global-set-key "\M-|" 'insert-or)

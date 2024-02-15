;;; .el --- ... -*- lexical-binding: t; -*-

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
                   (equal major-mode 'python-ts-mode)
                   (equal major-mode 'django-mode)
                   (equal major-mode 'shell-mode)
                   (equal major-mode 'cmake-mode))
               (insert (make-string 100 ?#)))
              ((or (equal major-mode 'c-mode)
                   (equal major-mode 'c++-mode)
                   (equal major-mode 'js-mode)
                   (equal major-mode 'js2-mode)
                   (equal major-mode 'rjsx-mode)
                   (equal major-mode 'scss-mode)
                   )
               (insert (concat "/" (make-string 98 ?*) "/")))
              ((string-match "\\.\\(sass\\|css\\)\\'" file)
               (insert (concat "//" (make-string 98 ?*))))
              ;; ((string-match "\\.\\(h\\|c\\|hpp\\|cpp\\|cc\\|sass\\|css\\)\\'" file)
              ;;  (insert (concat "/" (make-string 98 ?*) "/")))
              )
        ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Insert Short Rule for Python
;

(defun insert-short-rule ()
  (interactive)
  (save-excursion
    (let ((file (file-name-nondirectory buffer-file-name)))
      (progn
        (message "mode is [%s]" major-mode)
        (cond ((or (equal major-mode 'python-mode)
                   (equal major-mode 'python-ts-mode)
                   (equal major-mode 'django-mode))
               (insert (concat
                        (make-string 4 ? )
                        (make-string 46 ?#))))
              ((or (equal major-mode 'qml-mode))
               (insert (concat "    /" (make-string 54 ?*) "/")))
              )
        ))))

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

(defun insert-or ()
  (interactive)
  (insert "||"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Python Mode Kill Word up to underscore
;

(defun py-kill-word (arg)
  "Kill characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (kill-region (point) (progn (py-forward-into-nomenclature arg) (point))))

(defun py-backward-kill-word (arg)
  "Kill characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (kill-region (point) (progn (py-backward-into-nomenclature arg) (point))))

;; (defun my-delete-backward ()
;;   (interactive)
;;   (delete-region (point) (save-excursion (skip-syntax-backward "_") (point))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun recompile-elpa-packages()
  "Recompile all packages"
  (interactive)
  (byte-recompile-directory "~/.emacs.d/elpa" 0 t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-newlines-in-region ()
  "Removes all newlines in the region."
  (interactive)
  (save-restriction
    (narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (search-forward "\n" nil t) (replace-match "" nil t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ansi-color-on-buffer ()
  "..."
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(define-minor-mode ansi-color-mode
  "..."
  nil nil nil
  (ansi-color-apply-on-region 1 (buffer-size)))

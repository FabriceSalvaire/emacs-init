;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package flyspell-correct-ivy
;;   :after flyspell-correct
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fixme: ???
;; (autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
;; (autoload 'flyspell-delay-command "flyspell" "Delay on command." t)
;; (autoload 'tex-mode-flyspell-verify "flyspell" "" t)

;; (add-hook 'find-file-hook 'flyspell-on-for-buffer-type)

;; Flyspell will run a series of predicate functions to determine if a word should be spell checked.
;; (with-eval-after-load 'flyspell
;;   (progn
;;     (set-flyspell-predicate! '(markdown-mode gfm-mode)  #'+markdown-flyspell-word-p)
;;     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; https://github.com/emacsmirror/spell-fu

;;(use-package spell-fu
;;  :disabled
;;  :hook (text-mode . spell-fu-mode)
;;  :general ([remap ispell-word] #'+spell/correct)
;;  :preface
;;  (defvar +spell-correct-interface
;;    (cond ((modulep! :completion ivy)
;;           #'+spell-correct-ivy-fn)
;;          ((modulep! :completion helm)
;;           #'+spell-correct-helm-fn)
;;          (#'+spell-correct-generic-fn))
;;    "Function to use to display corrections.")
;;
;;  :init
;;  (defvar +spell-excluded-faces-alist
;;    '((markdown-mode
;;       . (markdown-code-face
;;          markdown-html-attr-name-face
;;          markdown-html-attr-value-face
;;          markdown-html-tag-name-face
;;          markdown-inline-code-face
;;          markdown-link-face
;;          markdown-markup-face
;;          markdown-plain-url-face
;;          markdown-reference-face
;;          markdown-url-face))
;;      (org-mode
;;       . (org-block
;;          org-block-begin-line
;;          org-block-end-line
;;          org-cite
;;          org-cite-key
;;          org-code
;;          org-date
;;          org-footnote
;;          org-formula
;;          org-inline-src-block
;;          org-latex-and-related
;;          org-link
;;          org-meta-line
;;          org-property-value
;;          org-ref-cite-face
;;          org-special-keyword
;;          org-tag
;;          org-todo
;;          org-todo-keyword-done
;;          org-todo-keyword-habt
;;          org-todo-keyword-kill
;;          org-todo-keyword-outd
;;          org-todo-keyword-todo
;;          org-todo-keyword-wait
;;          org-verbatim))
;;      (latex-mode
;;       . (font-latex-math-face
;;          font-latex-sedate-face
;;          font-lock-function-name-face
;;          font-lock-keyword-face
;;          font-lock-variable-name-face)))
;;    "Faces in certain major modes that spell-fu will not spellcheck.")
;;
;;  (setq spell-fu-directory (concat doom-data-dir "spell-fu"))
;;  (when (modulep! +everywhere)
;;    (add-hook! '(yaml-mode-hook
;;                 conf-mode-hook
;;                 prog-mode-hook)
;;               #'spell-fu-mode))
;;  :config
;;  ;; TODO PR this fix upstream!
;;  (defadvice! +spell--fix-face-detection-a (fn &rest args)
;;    "`spell-fu--faces-at-point' uses face detection that won't penetrary
;;overlays (like `hl-line'). This makes `spell-fu-faces-exclude' demonstrably less
;;useful when it'll still spellcheck excluded faces on any line that `hl-line' is
;;displayed on, even momentarily."
;;    :around #'spell-fu--faces-at-point
;;    (letf! (defun get-char-property (pos prop &optional obj)
;;             (or (plist-get (text-properties-at pos) prop)
;;                 (funcall get-char-property pos prop obj)))
;;      (apply fn args)))
;;
;;  (defadvice! +spell--create-word-dict-a (_word words-file _action)
;;    "Prevent `spell-fu--word-add-or-remove' from throwing non-existant
;;directory errors when writing a personal dictionary file (by creating the
;;directory first)."
;;    :before #'spell-fu--word-add-or-remove
;;    (unless (file-exists-p words-file)
;;      (make-directory (file-name-directory words-file) t)
;;      (with-temp-file words-file
;;        (insert (format "personal_ws-1.1 %s 0\n" ispell-dictionary)))))
;;
;;  (add-hook! 'spell-fu-mode-hook
;;    (defun +spell-init-excluded-faces-h ()
;;      "Set `spell-fu-faces-exclude' according to `+spell-excluded-faces-alist'."
;;      (when-let (excluded (cdr (cl-find-if #'derived-mode-p +spell-excluded-faces-alist :key #'car)))
;;        (setq-local spell-fu-faces-exclude excluded))))
;;
;;  ;; TODO custom `spell-fu-check-range' function to reduce false positives
;;  ;;      more intelligently, or modify `spell-fu-word-regexp' to include
;;  ;;      non-latin charactersets.
;;  )

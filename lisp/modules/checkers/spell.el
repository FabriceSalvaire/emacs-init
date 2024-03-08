;;; checkers/spell/config.el -*- lexical-binding: t; -*-

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Spelling.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ispell

(defun fr ()
  "Use fr_FR dictionary"
  (interactive)
  (progn
    ;; (setq ispell-local-dictionary "")
    (ispell-change-dictionary "fr_FR")))

(defun gb ()
  "Use gb dictionary"
  (interactive)
  (progn
    (ispell-change-dictionary "en_GB")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fixme: ???
;; `elisp-mode' is loaded at startup.
;; In order to lazy load its config we need to pretend it isn't loaded
(delq! 'ispell features)

(global-set-key [remap ispell-word] #'+spell/correct)

(after! ispell
  ;; Enchant is a library (and command-line program) that wraps a number of different spelling
  ;;   libraries and programs with a consistent interface.
  ;;   https://abiword.github.io/enchant
  ;; https://hunspell.github.io
  ;; https://nuspell.github.io

  ;; /usr/bin/
  ;;  (setq ispell-program-name "hunspell"))
  (setq ispell-program-name "enchant-2")

  (setq ispell-hunspell-dict-paths-alist
        '(("en_GB" "/usr/share/hunspell/en_GB.aff")
          ("fr_FR" "/usr/share/hunspell/fr_FR.aff")
          ))
  (ispell-change-dictionary "en_GB" "globally")

  ;; Don't spellcheck org blocks
  (pushnew! ispell-skip-region-alist
            '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
            '("#\\+BEGIN_SRC" . "#\\+END_SRC")
            '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))
)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Flyspell
;;   It is significantly slower than spell-fu, but supports multiple languages and dictionaries.
;;   https://www.emacswiki.org/emacs/FlySpell

(load "checkers/flyspell.autoload.el")

(use-package flyspell ; built-in
  :defer t
  :preface
  ;; Fixme: ???
  ;; `flyspell' is loaded at startup.
  ;; In order to lazy load its config we need to pretend it isn't loaded.
  (defer-feature! flyspell flyspell-mode flyspell-prog-mode)
  :init
  (add-hook! '(org-mode-hook
               markdown-mode-hook
               TeX-mode-hook
               rst-mode-hook
               mu4e-compose-mode-hook
               message-mode-hook
               git-commit-mode-hook)
             #'flyspell-mode)

  (add-hook! '(yaml-mode-hook
               conf-mode-hook
               prog-mode-hook)
             #'flyspell-prog-mode)

  :config
  (provide 'ispell) ; forcibly load ispell configs

  (setq flyspell-issue-welcome-flag nil
        ;; Significantly speeds up flyspell, which would otherwise print
        ;; messages for every word when checking the entire buffer
        flyspell-issue-message-flag nil)

  (add-hook! 'flyspell-mode-hook
    (defun +spell-inhibit-duplicate-detection-maybe-h ()
      "Don't mark duplicates when style/grammar linters are present.
e.g. proselint and langtool."
      (and (or (and (bound-and-true-p flycheck-mode)
                    (executable-find "proselint"))
               (featurep 'langtool))
           (setq-local flyspell-mark-duplications-flag nil))))

  ;; Ensure mode-local predicates declared with `set-flyspell-predicate!' are
  ;; used in their respective major modes.
  (add-hook 'flyspell-mode-hook #'+spell-init-flyspell-predicate-h)

  (let ((flyspell-correct
         (cmds! (and (not mark-active)
                     (not (and (bound-and-true-p evil-local-mode)
                               (or (evil-insert-state-p)
                                   (evil-emacs-state-p))))
                     (memq 'flyspell-incorrect (face-at-point nil t)))
                #'flyspell-correct-at-point)))
    (map! :map flyspell-mouse-map
          "RET"    flyspell-correct
          [return] flyspell-correct
          [mouse-1] #'flyspell-correct-at-point))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; flyspell-correct
;;   https://github.com/d12frosted/flyspell-correct
;;   Distraction-free words correction with flyspell via selected interface.

(use-package flyspell-correct
  :commands flyspell-correct-previous
  :general ([remap ispell-word] #'flyspell-correct-at-point)
  :config
  ;; (require 'flyspell-correct-helm nil t)
  ;; (require 'flyspell-correct-ivy nil t)
  ;; vertico doesn't need any extra configuration
  (require 'flyspell-correct-popup nil t) ; only use popup if no compatible completion UI is enabled
  (setq flyspell-popup-correct-delay 0.8)
  (define-key popup-menu-keymap [escape] #'keyboard-quit)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; flyspell-lazy
;;   Improve Emacs flyspell responsiveness using idle timers
;;   https://github.com/rolandwalker/flyspell-lazy

(use-package flyspell-lazy
  :after flyspell
  :config
  (setq flyspell-lazy-idle-seconds 1
        flyspell-lazy-window-idle-seconds 3)
  ;; Fix #3357: flyspell-lazy inhibits flyspell entirely in message-mode
  ;; derivatives (e.g. for notmuch users).
  (setq-hook! 'message-mode-hook flyspell-lazy-disallow-buffers nil)
  (flyspell-lazy-mode +1)
  )

;;; completion/company/config.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defvar +company-backend-alist
  '((text-mode (:separate company-dabbrev company-yasnippet company-ispell))
    (prog-mode company-capf company-yasnippet)
    (conf-mode company-capf company-dabbrev-code company-yasnippet))
  "An alist matching modes to company backends. The backends for any mode is
built from this.")

;;;###autodef
(defun set-company-backend! (modes &rest backends)
  "Prepends BACKENDS (in order) to `company-backends' in MODES.

MODES should be one symbol or a list of them, representing major or minor modes.
This will overwrite backends for MODES on consecutive uses.

If the car of BACKENDS is nil, unset the backends for MODES.

Examples:

  (set-company-backend! 'js2-mode
    'company-tide 'company-yasnippet)

  (set-company-backend! 'sh-mode
    '(company-shell :with company-yasnippet))

  (set-company-backend! '(c-mode c++-mode)
    '(:separate company-irony-c-headers company-irony))

  (set-company-backend! 'sh-mode nil)  ; unsets backends for sh-mode"
  (declare (indent defun))
  (dolist (mode (ensure-list modes))
    (if (null (car backends))
        (setq +company-backend-alist
              (delq (assq mode +company-backend-alist)
                    +company-backend-alist))
      (setf (alist-get mode +company-backend-alist)
            backends))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Library

(defun +company--backends ()
  (let (backends)
    (let ((mode major-mode)
          (modes (list major-mode)))
      (while (setq mode (get mode 'derived-mode-parent))
        (push mode modes))
      (dolist (mode modes)
        (dolist (backend (append (cdr (assq mode +company-backend-alist))
                                 (default-value 'company-backends)))
          (push backend backends)))
      (delete-dups
       (append (cl-loop for (mode . backends) in +company-backend-alist
                        if (or (eq major-mode mode)  ; major modes
                               (and (boundp mode)
                                    (symbol-value mode))) ; minor modes
                        append backends)
               (nreverse backends))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Hooks

;;;###autoload
(defun +company-init-backends-h ()
  "Set `company-backends' for the current buffer."
  (or (memq major-mode '(fundamental-mode special-mode))
      buffer-read-only
      (doom-temp-buffer-p (or (buffer-base-buffer) (current-buffer)))
      (setq-local company-backends (+company--backends))))

(put '+company-init-backends-h 'permanent-local-hook t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Commands

;;;###autoload
(defun +company-has-completion-p ()
  "Return non-nil if a completion candidate exists at point."
  (when company-mode
    (unless company-candidates-length
      (company-manual-begin))
    (= company-candidates-length 1)))

;;;###autoload
(defun +company/toggle-auto-completion ()
  "Toggle as-you-type code completion."
  (interactive)
  (require 'company)
  (setq company-idle-delay (unless company-idle-delay 0.2))
  (message "Auto completion %s"
           (if company-idle-delay "enabled" "disabled")))

;;;###autoload
(defun +company/complete ()
  "Bring up the completion popup. If only one result, complete it."
  (interactive)
  (require 'company)
  (when (ignore-errors
          (/= (point)
              (cdr (bounds-of-thing-at-point 'symbol))))
    (save-excursion (insert " ")))
  (when (and (company-manual-begin)
             (= company-candidates-length 1))
    (company-complete-common)))

;;;###autoload
(defun +company/dabbrev ()
  "Invokes `company-dabbrev-code' in prog-mode buffers and `company-dabbrev'
everywhere else."
  (interactive)
  (call-interactively
   (if (derived-mode-p 'prog-mode)
       #'company-dabbrev-code
     #'company-dabbrev)))

;;;###autoload
(defun +company/whole-lines (command &optional arg &rest ignored)
  "`company-mode' completion backend that completes whole-lines, akin to vim's
C-x C-l."
  (interactive (list 'interactive))
  (require 'company)
  (pcase command
    (`interactive (company-begin-backend '+company/whole-lines))
    (`prefix      (company-grab-line "^[\t\s]*\\(.+\\)" 1))
    (`candidates
     (all-completions
      arg
      (delete-dups
       (split-string
        (replace-regexp-in-string
         "^[\t\s]+" ""
         (concat (buffer-substring-no-properties (point-min) (line-beginning-position))
                 (buffer-substring-no-properties (line-end-position) (point-max))))
        "\\(\r\n\\|[\n\r]\\)" t))))))

;;;###autoload
(defun +company/dict-or-keywords ()
  "`company-mode' completion combining `company-dict' and `company-keywords'."
  (interactive)
  (require 'company-dict)
  (require 'company-keywords)
  (let ((company-backends '((company-keywords company-dict))))
    (call-interactively #'company-complete)))

;;;###autoload
(defun +company/dabbrev-code-previous ()
  "TODO"
  (interactive)
  (require 'company-dabbrev)
  (let ((company-selection-wrap-around t))
    (call-interactively #'+company/dabbrev)
    (company-select-previous-or-abort)))

;;;###autoload
(defun +company/completing-read ()
  "Complete current company candidates in minibuffer.

Uses ivy, helm, vertico, or ido, if available."
  (interactive)
  (cond ;; ((modulep! :completion ivy)
        ;;  (call-interactively #'counsel-company))
        ;; ((modulep! :completion helm)
        ;;  (call-interactively #'helm-company))
        ((not company-candidates)
         (user-error "No company candidates available"))
        ((when-let (cand (completing-read "Candidate: " company-candidates))
           (company-finish cand)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Company — Modular in-buffer completion framework
;;   https://company-mode.github.io
;;   https://github.com/company-mode/company-mode
;;
;;   Company is a text completion framework.
;;   It uses pluggable back-ends and front-ends to retrieve and display completion candidates.
;;   It comes with several back-ends such as Elisp, Clang, Semantic, Ispell, CMake, BBDB, Yasnippet,
;;   Dabbrev, Etags, Gtags, Files, Keywords and a few others.
;;   The CAPF back-end provides a bridge to the standard completion-at-point-functions facility, and
;;   thus works with any major mode that defines a proper completion function.

(use-package company
  :commands
  company-complete-common
  company-complete-common-or-cycle
  company-manual-begin
  company-grab-line
  :hook (doom-first-input . global-company-mode)

  :init
  (setq company-minimum-prefix-length 2
        company-tooltip-limit 14
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes
        '(not erc-mode
              circe-mode
              message-mode
              help-mode
              gud-mode
              vterm-mode)
        company-frontends
        '(company-pseudo-tooltip-frontend  ; always show candidates in overlay tooltip
          company-echo-metadata-frontend)  ; show selected candidate docs in echo area

        ;; Buffer-local backends will be computed when loading a major mode, so
        ;; only specify a global default here.
        company-backends '(company-capf)

        ;; These auto-complete the current selection when
        ;; `company-auto-commit-chars' is typed. This is too magical. We
        ;; already have the much more explicit RET and TAB.
        company-auto-commit nil

        ;; Only search the current buffer for `company-dabbrev' (a backend that
        ;; suggests text your open buffers). This prevents Company from causing
        ;; lag once you have a lot of buffers open.
        company-dabbrev-other-buffers nil
        ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
        ;; domain-specific words with particular casing.
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        )

  ;; company-mode configuration for single-button interaction
  ;; (when (modulep! +tng)
  ;;   (add-hook 'global-company-mode-hook #'company-tng-mode))

  :config
  ;; (when (modulep! :editor evil)
  ;;   (add-hook 'company-mode-hook #'evil-normalize-keymaps)
  ;;   (add-hook! 'evil-normal-state-entry-hook
  ;;     (defun +company-abort-h ()
  ;;       ;; HACK `company-abort' doesn't no-op if company isn't active; causing
  ;;       ;;      unwanted side-effects, like the suppression of messages in the
  ;;       ;;      echo-area.
  ;;       ;; REVIEW Revisit this to refactor; shouldn't be necessary!
  ;;       (when company-candidates
  ;;         (company-abort))))
  ;;   ;; Allow users to switch between backends on the fly. E.g. C-x C-s followed
  ;;   ;; by C-x C-n, will switch from `company-yasnippet' to
  ;;   ;; `company-dabbrev-code'.
  ;;   (defadvice! +company--abort-previous-a (&rest _)
  ;;     :before #'company-begin-backend
  ;;     (company-abort)))

  (add-hook 'after-change-major-mode-hook #'+company-init-backends-h 'append)

  ;; NOTE Fix #1335: ensure `company-emulation-alist' is the first item of
  ;;      `emulation-mode-map-alists', thus higher priority than keymaps of
  ;;      evil-mode. We raise the priority of company-mode keymaps
  ;;      unconditionally even when completion is not activated. This should not
  ;;      cause problems, because when completion is activated, the value of
  ;;      `company-emulation-alist' is ((t . company-my-keymap)), when
  ;;      completion is not activated, the value is ((t . nil)).
  ;; (add-hook! 'evil-local-mode-hook
  ;;   (when (memq 'company-emulation-alist emulation-mode-map-alists)
  ;;     (company-ensure-emulation-alist)))

  ;; eldoc-mode is a minor mode which shows you, in the echo area,
  ;; the argument list of the function call you are currently writing.
  ;;   https://www.emacswiki.org/emacs/ElDoc
  ;; Fix #4355: allow eldoc to trigger after completions.
  (after! eldoc
    (eldoc-add-command 'company-complete-selection
                       'company-complete-common
                       'company-capf
                       'company-abort))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! company-files
  ;; Fix `company-files' completion for org file:* links
  (add-to-list 'company-files--regexps "file:\\(\\(?:\\.\\{1,2\\}/\\|~/\\|/\\)[^\]\n]*\\)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; company-box — company front-end with icons
;;  https://github.com/sebastiencs/company-box
;;  - Differents colors for differents backends
;;  - Icons associated to functions/variables/.. and their backends
;;  - Display candidate's documentation (support quickhelp-string)
;;  - Not limited by the current window size, buffer's text properties

(use-package company-box
  :disabled
  ;; :when (modulep! +childframe)
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-show-single-candidate t
        company-box-backends-colors nil
        company-box-tooltip-limit 50
        company-box-icons-alist 'company-box-icons-nerd-icons
        ;; Move company-box-icons--elisp to the end, because it has a catch-all
        ;; clause that ruins icons from other backends in elisp buffers.
        company-box-icons-functions
        (cons #'+company-box-icons--elisp-fn
              (delq 'company-box-icons--elisp
                    company-box-icons-functions))
        company-box-icons-nerd-icons
        `((Unknown        . ,(nerd-icons-codicon  "nf-cod-code"                :face  'font-lock-warning-face))
          (Text           . ,(nerd-icons-codicon  "nf-cod-text_size"           :face  'font-lock-doc-face))
          (Method         . ,(nerd-icons-codicon  "nf-cod-symbol_method"       :face  'font-lock-function-name-face))
          (Function       . ,(nerd-icons-codicon  "nf-cod-symbol_method"       :face  'font-lock-function-name-face))
          (Constructor    . ,(nerd-icons-codicon  "nf-cod-triangle_right"      :face  'font-lock-function-name-face))
          (Field          . ,(nerd-icons-codicon  "nf-cod-symbol_field"        :face  'font-lock-variable-name-face))
          (Variable       . ,(nerd-icons-codicon  "nf-cod-symbol_variable"     :face  'font-lock-variable-name-face))
          (Class          . ,(nerd-icons-codicon  "nf-cod-symbol_class"        :face  'font-lock-type-face))
          (Interface      . ,(nerd-icons-codicon  "nf-cod-symbol_interface"    :face  'font-lock-type-face))
          (Module         . ,(nerd-icons-codicon  "nf-cod-file_submodule"      :face  'font-lock-preprocessor-face))
          (Property       . ,(nerd-icons-codicon  "nf-cod-symbol_property"     :face  'font-lock-variable-name-face))
          (Unit           . ,(nerd-icons-codicon  "nf-cod-symbol_ruler"        :face  'font-lock-constant-face))
          (Value          . ,(nerd-icons-codicon  "nf-cod-symbol_field"        :face  'font-lock-builtin-face))
          (Enum           . ,(nerd-icons-codicon  "nf-cod-symbol_enum"         :face  'font-lock-builtin-face))
          (Keyword        . ,(nerd-icons-codicon  "nf-cod-symbol_keyword"      :face  'font-lock-keyword-face))
          (Snippet        . ,(nerd-icons-codicon  "nf-cod-symbol_snippet"      :face  'font-lock-string-face))
          (Color          . ,(nerd-icons-codicon  "nf-cod-symbol_color"        :face  'success))
          (File           . ,(nerd-icons-codicon  "nf-cod-symbol_file"         :face  'font-lock-string-face))
          (Reference      . ,(nerd-icons-codicon  "nf-cod-references"          :face  'font-lock-variable-name-face))
          (Folder         . ,(nerd-icons-codicon  "nf-cod-folder"              :face  'font-lock-variable-name-face))
          (EnumMember     . ,(nerd-icons-codicon  "nf-cod-symbol_enum_member"  :face  'font-lock-builtin-face))
          (Constant       . ,(nerd-icons-codicon  "nf-cod-symbol_constant"     :face  'font-lock-constant-face))
          (Struct         . ,(nerd-icons-codicon  "nf-cod-symbol_structure"    :face  'font-lock-variable-name-face))
          (Event          . ,(nerd-icons-codicon  "nf-cod-symbol_event"        :face  'font-lock-warning-face))
          (Operator       . ,(nerd-icons-codicon  "nf-cod-symbol_operator"     :face  'font-lock-comment-delimiter-face))
          (TypeParameter  . ,(nerd-icons-codicon  "nf-cod-list_unordered"      :face  'font-lock-type-face))
          (Template       . ,(nerd-icons-codicon  "nf-cod-symbol_snippet"      :face  'font-lock-string-face))
          (ElispFunction  . ,(nerd-icons-codicon  "nf-cod-symbol_method"       :face  'font-lock-function-name-face))
          (ElispVariable  . ,(nerd-icons-codicon  "nf-cod-symbol_variable"     :face  'font-lock-variable-name-face))
          (ElispFeature   . ,(nerd-icons-codicon  "nf-cod-globe"               :face  'font-lock-builtin-face))
          (ElispFace      . ,(nerd-icons-codicon  "nf-cod-symbol_color"        :face  'success))))

  ;; HACK Fix oversized scrollbar in some odd cases
  ;; REVIEW `resize-mode' is deprecated and may stop working in the future.
  ;; TODO PR me upstream?
  (setq x-gtk-resize-child-frames 'resize-mode)

  ;; Disable tab-bar in company-box child frames
  ;; TODO PR me upstream!
  (add-to-list 'company-box-frame-parameters '(tab-bar-lines . 0))

  ;; Don't show documentation in echo area, because company-box displays its own
  ;; in a child frame.
  (delq! 'company-echo-metadata-frontend company-frontends)

  (defun +company-box-icons--elisp-fn (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym)  'ElispFunction)
              ((boundp sym)   'ElispVariable)
              ((featurep sym) 'ElispFeature)
              ((facep sym)    'ElispFace)))))

  ;; `company-box' performs insufficient frame-live-p checks. Any command that
  ;; "cleans up the session" will break company-box.
  ;; TODO Fix this upstream.
  (defadvice! +company-box-detect-deleted-frame-a (frame)
    :filter-return #'company-box--get-frame
    (if (frame-live-p frame) frame))
  (defadvice! +company-box-detect-deleted-doc-frame-a (_selection frame)
    :before #'company-box-doc
    (and company-box-doc-enable
         (frame-local-getq company-box-doc-frame frame)
         (not (frame-live-p (frame-local-getq company-box-doc-frame frame)))
         (frame-local-setq company-box-doc-frame nil frame)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; company-dict
;;   A port of ac-source-dictionary to company-mode, plus annotation and documentation support.
;;   https://github.com/hlissner/emacs-company-dict
;;   This simple company backend emulates ac-source-dictionary and vim's dictionary omnicompletion
;;   (C-x C-k). It's much like company-keywords, but with lazy-loaded dictionary files, and support
;;   for annotations and documentation.

(use-package company-dict
  :defer t
  :config
  ;; (setq company-dict-dir (expand-file-name "dicts" doom-user-dir))
  ;; Fixme: doom
  (add-hook! 'doom-project-hook
    (defun +company-enable-project-dicts-h (mode &rest _)
      "Enable per-project dictionaries."
      (if (symbol-value mode)
          (add-to-list 'company-dict-minor-mode-list mode nil #'eq)
        (setq company-dict-minor-mode-list (delq mode company-dict-minor-mode-list)))))
  )

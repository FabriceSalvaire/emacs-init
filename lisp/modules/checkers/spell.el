;;; checkers/spell/config.el -*- lexical-binding: t; -*-

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Spelling.html
;; M-$ ispell-word

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "checkers/flyspell.autoload.el")

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
  ;; Fixme: :config ???
  (add-hook! '(
               TeX-mode-hook
               git-commit-mode-hook
               markdown-mode-hook
               message-mode-hook
               mu4e-compose-mode-hook
               org-mode-hook
               rst-mode-hook
               ; LaTeX-mode-hook
               )
             #'flyspell-mode)

  (add-hook! '(
               c++-mode-hook
               c-mode-hook
               conf-mode-hook
               java-mode-hook
               javascript-mode-hook
               prog-mode-hook
               python-mode-hook
               qml-mode-hook
               yaml-mode-hook
               )
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

(defun flyspell-on-for-buffer-type ()
  "Enable Flyspell appropriately for the major mode of the current buffer.  Uses `flyspell-prog-mode' for modes derived from `prog-mode', so only strings and comments get checked.  All other buffers get `flyspell-mode' to check all text.  If flyspell is already enabled, does nothing."
  (interactive)
  (if (not (symbol-value flyspell-mode)) ; if not already on
      (progn
        (if (derived-mode-p 'prog-mode)
            (progn
              (message "Flyspell on (code)")
              (flyspell-prog-mode))
          ;; else
          (progn
            (message "Flyspell on (text)")
            (flyspell-mode 1)))
        ;; I tried putting (flyspell-buffer) here but it didn't seem to work
        )))


(defun flyspell-toggle ()
  "Turn Flyspell on if it is off, or off if it is on.  When turning on, it uses `flyspell-on-for-buffer-type' so code-vs-text is handled appropriately."
  (interactive)
  (if (symbol-value flyspell-mode)
      (progn ; flyspell is on, turn it off
        (message "Flyspell off")
        (flyspell-mode -1))
    ;; else - flyspell is off, turn it on
    (flyspell-on-for-buffer-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; flyspell-correct
;;   https://github.com/d12frosted/flyspell-correct
;;   Distraction-free words correction with flyspell via selected interface.
;;   Correcting misspelled words with flyspell using favourite interface.

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

  ;; :after flyspell
  ;; :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; flyspell-lazy
;;   Improve Emacs flyspell responsiveness using idle timers
;;   https://github.com/rolandwalker/flyspell-lazy

(use-package flyspell-lazy
  :after flyspell
  ;; Fixme:
  ;;   :custom
  ;;   (flyspell-lazy-idle-seconds 2)
  :config
  (setq flyspell-lazy-idle-seconds 1
        flyspell-lazy-window-idle-seconds 3)
  ;; Fix #3357: flyspell-lazy inhibits flyspell entirely in message-mode
  ;; derivatives (e.g. for notmuch users).
  (setq-hook! 'message-mode-hook flyspell-lazy-disallow-buffers nil)
  (flyspell-lazy-mode +1) ; Fixme: +1 ???
  )

;;; .el --- ... -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; see Doom  modules/checkers/spell

(use-package emacs
  :config

  ;; https://nuspell.github.io
  ; (setq-default ispell-program-name "/usr/bin/nuspell")
  (setq-default ispell-program-name "/usr/bin/hunspell")
  (setq ispell-hunspell-dict-paths-alist
        '(("en_GB" "/usr/share/hunspell/en_GB.aff")
          ("fr_FR" "/usr/share/hunspell/fr_FR.aff")
          ))
  (ispell-change-dictionary "en_GB" "globally")
)

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
;;
;; FlySpell
;;   https://www.emacswiki.org/emacs/FlySpell
;;
;; M-$ ispell-word

(use-package flyspell
  :config
  (dolist (hook
           '(
             text-mode-hook
             LaTeX-mode-hook
             markdown-mode
             )
           )
    (add-hook hook (lambda () (flyspell-mode 1))))
  (dolist (hook
           '(
             c-mode-hook
             java-mode-hook
             javascript-mode-hook
             python-mode-hook
             qml-mode-hook
             c++-mode-hook
             )
           )
    (add-hook hook (lambda () (flyspell-prog-mode))))
  )


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
;; https://github.com/rolandwalker/flyspell-lazy
;;   Improve Emacs flyspell responsiveness using idle timers.

(use-package flyspell-lazy
  :ensure t
  :after flyspell
  :custom
  (flyspell-lazy-idle-seconds 2)
  :config
  (flyspell-lazy-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; https://github.com/d12frosted/flyspell-correct
;;   Correcting misspelled words with flyspell using favourite interface.

(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper))
  )

(use-package flyspell-correct-ivy
  :after flyspell-correct
  )

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
;; https://github.com/PillFall/languagetool.el
;;   https://github.com/PillFall/Emacs-LanguageTool.el
;;   https://github.com/mhayashi1120/Emacs-langtool

(use-package languagetool
  :ensure t
  :commands (languagetool-check
             languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
             languagetool-set-language
             languagetool-server-mode
             languagetool-server-start
             languagetool-server-stop)
  :config
  (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8")
        languagetool-console-command "/usr/local/stow/LanguageTool/languagetool-commandline.jar"
        languagetool-server-command  "/usr/local/stow/LanguageTool/languagetool-server.jar"
        languagetool-default-language "en-GB"
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; flycheck-languagetool
;;   https://github.com/emacs-languagetool/flycheck-languagetool
;;   https://dev.languagetool.org/http-server

(use-package flycheck-languagetool
  :ensure t
  :hook (text-mode . (lambda () (require 'flycheck-languagetool)))
  :init
  (setq flycheck-languagetool-commandline-jar "/usr/local/stow/LanguageTool/languagetool-commandline.jar"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Google Translate

(use-package google-translate
  :ensure t
  :defer t
  :custom
  (google-translate-translation-directions-alist '(("en" . "fr") ("fr" . "en")))
  :config
  ;; (use-package google-translate-default-ui)
  (use-package google-translate-smooth-ui)
  )

;;; checkers/grammar/config.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Language Tool
;;   https://github.com/PillFall/Emacs-LanguageTool.el
;;   https://github.com/mhayashi1120/Emacs-langtool

(use-package langtool
  :defer t
  :commands (langtool-check
             langtool-check-done
             langtool-correct-buffer
             langtool-show-message-at-point
             ;; languagetool-clear-suggestions
             ;; languagetool-correct-at-point
             ;; languagetool-set-language
             ;; languagetool-server-mode
             ;; languagetool-server-start
             ;; languagetool-server-stop
             )
  :init
  (setq langtool-default-language "en-GB")
  :config
  (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8")
        languagetool-console-command "/usr/local/stow/LanguageTool/languagetool-commandline.jar"
        languagetool-server-command  "/usr/local/stow/LanguageTool/languagetool-server.jar"
        )
  ;; Fixme: Doom
  ;; (unless (or langtool-bin
  ;;             langtool-language-tool-jar
  ;;             langtool-java-classpath)
  ;;   (cond ((setq langtool-bin
  ;;                (or (executable-find "languagetool-commandline")
  ;;                    (executable-find "languagetool"))))  ; for nixpkgs.languagetool
  ;;         ((featurep :system 'linux)
  ;;          (setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*"))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; flycheck-languagetool
;;   https://github.com/emacs-languagetool/flycheck-languagetool
;;   https://dev.languagetool.org/http-server

;; (use-package flycheck-languagetool
;;   :disabled
;;   :hook (text-mode . (lambda () (require 'flycheck-languagetool)))
;;   :init
;;   (setq flycheck-languagetool-commandline-jar "/usr/local/stow/LanguageTool/languagetool-commandline.jar"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Writegood Mode
;;   https://github.com/bnbeckwith/writegood-mode
;; Detects weasel words, passive voice and duplicates.

;; Proselint would be a better choice.
;; https://github.com/amperser/proselint
;; Emacs via Flycheck or via Flymake

(use-package writegood-mode
  :disabled
  :hook (asciidoc-mode
         latex-mode
         LaTeX-mode
         markdown-mode
         org-mode
         rst-mode)
  :config
  ;; (map! :localleader
  ;;       :map writegood-mode-map
  ;;       "g" #'writegood-grade-level
  ;;       "r" #'writegood-reading-ease)
  )

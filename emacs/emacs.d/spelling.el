;; see Doom  modules/checkers/spell

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Aspell

(setq-default ispell-program-name "aspell")
(ispell-change-dictionary "british" "globally")
;;
(add-to-list 'ispell-extra-args "--sug-mode=ultra")

(defun fr ()
  "Use fr_FR dictionary"
  (interactive)
  (progn
    (ispell-change-dictionary "francais")))
    ;(ispell-change-dictionary "fr_FR")))

(defun gb ()
  "Use gb dictionary"
  (interactive)
  (progn
    (ispell-change-dictionary "british")))
    ;(ispell-change-dictionary "en_GB-ise-w_accents")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FlySpell
;;   https://www.emacswiki.org/emacs/FlySpell

(use-package flyspell)

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

;; M-$ ispell-word

;; https://github.com/rolandwalker/flyspell-lazy
;;   Improve Emacs flyspell responsiveness using idle timers.
(use-package flyspell-lazy
  :custom
  (flyspell-lazy-idle-seconds 2)

  :config
  (flyspell-lazy-mode 1)
  )

;; https://github.com/d12frosted/flyspell-correct
;;   Correcting misspelled words with flyspell using favourite interface.
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))
(use-package flyspell-correct-ivy
  :after flyspell-correct)

(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." t)
(autoload 'tex-mode-flyspell-verify "flyspell" "" t)

;; (add-hook 'find-file-hook 'flyspell-on-for-buffer-type)

;; Flyspell will run a series of predicate functions to determine if a word should be spell checked.
;; (with-eval-after-load 'flyspell
;;   (progn
;;     (set-flyspell-predicate! '(markdown-mode gfm-mode)  #'+markdown-flyspell-word-p)
;;     ))

(dolist (hook
         '(text-mode-hook
           LaTeX-mode-hook
           markdown-mode
	   )
	 )
  (add-hook hook (lambda () (flyspell-mode 1))))

(dolist (hook
         '(c-mode-hook
           c++-mode-hook
	   java-mode-hook
	   javascript-mode-hook
	   qml-mode-hook
	   python-mode-hook
	   )
	 )
  (add-hook hook (lambda () (flyspell-prog-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Google Translate

(use-package google-translate
  :custom
  (google-translate-translation-directions-alist '(("en" . "fr") ("fr" . "en")))
  )
;; (use-package google-translate-default-ui)
(use-package google-translate-smooth-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; https://github.com/PillFall/Emacs-LanguageTool.el

(setq languagetool-language-tool-jar "/usr/local/stow/LanguageTool-5.3/languagetool-commandline.jar")
;; (setq languagetool-server-language-tool-jar "/usr/local/stow/LanguageTool-5.3/languagetool-server.jar")
;; (languagetool-server-start)
(setq languagetool-java-arguments '("-Dfile.encoding=UTF-8"))
(setq languagetool-default-language "en-GB")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; flycheck-languagetool
;;   https://github.com/emacs-languagetool/flycheck-languagetool
;;   https://dev.languagetool.org/http-server

(use-package flycheck-languagetool
  :ensure t
  :hook (text-mode . (lambda () (require 'flycheck-languagetool)))
  :init
  (setq flycheck-languagetool-commandline-jar "/usr/local/stow/LanguageTool-5.3/languagetool-commandline.jar"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; hunspell

;; (setq-default ispell-program-name "hunspell")
;; (ispell-change-dictionary "fr_FR")
;; (setq ispell-extra-args '("-a" "-i" "utf-8"))

;; (require 'ispell)
;; (eval-after-load "ispell"
;;   ;; so that following modifications won't be lost when ispell is loaded
;;   '(progn
;;
;;      ;; default dictionary to use (if `ispell-local-dictionary' is nil)
;;      ; (setq ispell-dictionary "fr_FR")
;;
;;      ;; save the personal dictionary without confirmation
;;      ; (setq ispell-silently-savep t)
;;
;;      ;; extra switches to pass to the `ispell' program
;;      ;; TODO Check they are right!
;;      ; /usr/bin/hunspell -a  -B -i iso-8859-1
;;      (setq ispell-extra-args '("-a" "-i" "utf-8"))
;;
;;      ;; redefine the list of installed dictionaries
;;      ;; ??? FIXME This variable is reset once latter in this .emacs file!!!
;;      (setq ispell-dictionary-alist
;;      	   ;; those not here will be "undefined dictionary"
;;      	   '(
;;      	     ;; default
;;      	     (nil
;;      	      "[A-Za-z]" "[^A-Za-z]"
;;      	      "[']" nil ("-B") nil iso-8859-1)
;;
;;      	     ;; US English
;;      	     ("en_US"
;;      	      "[A-Za-z]" "[^A-Za-z]"
;;      	      "[']" nil ("-B") nil utf-8)
;;
;;      	     ;; standard French
;;      	     ("fr_FR"
;;      	      "[a-zàâäéèêëîïôöùûüçA-ZÀÂÄÉÈÊËÎÏÔÖÙÛÜÇ]" "[^a-zàâäéèêëîïôöùûüçA-ZÀÂÄÉÈÊËÎÏÔÖÙÛÜÇ]"
;;      	      "[-']" t nil "~list" utf-8)
;;      	     ))
;;
;;      ;; `aspell' extensions should *not* be used
;;      (setq ispell-really-aspell nil)
;;
;;      ;; `hunspell' extensions should be used
;;      (setq ispell-really-hunspell t)
;;      )
;; )

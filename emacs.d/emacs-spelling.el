;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Aspell

(setq-default ispell-program-name "aspell")
(ispell-change-dictionary "british" "globally")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Flyspell

(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." t)
(autoload 'tex-mode-flyspell-verify "flyspell" "" t)

(add-hook 'LaTeX-mode-hook 'flyspell-mode)

(dolist (hook
         '(text-mode-hook)
         '(LaTeX-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(dolist (hook
         '(python-mode-hook)
         '(c-mode-hook)
         '(c++-mode-hook))
  (add-hook hook (lambda () (flyspell-prog-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; hunspell

;;; (setq-default ispell-program-name "hunspell")
;;; (ispell-change-dictionary "fr_FR")
;;; (setq ispell-extra-args '("-a" "-i" "utf-8"))

;;; (require 'ispell)
;;; (eval-after-load "ispell"
;;;   ;; so that following modifications won't be lost when ispell is loaded
;;;   '(progn
;;;
;;;      ;; default dictionary to use (if `ispell-local-dictionary' is nil)
;;;      ; (setq ispell-dictionary "fr_FR")
;;;
;;;      ;; save the personal dictionary without confirmation
;;;      ; (setq ispell-silently-savep t)
;;;
;;;      ;; extra switches to pass to the `ispell' program
;;;      ;; TODO Check they are right!
;;;      ; /usr/bin/hunspell -a  -B -i iso-8859-1
;;;      (setq ispell-extra-args '("-a" "-i" "utf-8"))
;;;
;;;      ;; redefine the list of installed dictionaries
;;;      ;; ??? FIXME This variable is reset once latter in this .emacs file!!!
;;;      (setq ispell-dictionary-alist
;;;      	   ;; those not here will be "undefined dictionary"
;;;      	   '(
;;;      	     ;; default
;;;      	     (nil
;;;      	      "[A-Za-z]" "[^A-Za-z]"
;;;      	      "[']" nil ("-B") nil iso-8859-1)
;;;
;;;      	     ;; US English
;;;      	     ("en_US"
;;;      	      "[A-Za-z]" "[^A-Za-z]"
;;;      	      "[']" nil ("-B") nil utf-8)
;;;
;;;      	     ;; standard French
;;;      	     ("fr_FR"
;;;      	      "[a-zàâäéèêëîïôöùûüçA-ZÀÂÄÉÈÊËÎÏÔÖÙÛÜÇ]" "[^a-zàâäéèêëîïôöùûüçA-ZÀÂÄÉÈÊËÎÏÔÖÙÛÜÇ]"
;;;      	      "[-']" t nil "~list" utf-8)
;;;      	     ))
;;;
;;;      ;; `aspell' extensions should *not* be used
;;;      (setq ispell-really-aspell nil)
;;;
;;;      ;; `hunspell' extensions should be used
;;;      (setq ispell-really-hunspell t)
;;;      )
;;; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; End
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

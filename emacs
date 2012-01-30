; -*- lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                                                   ;
;                                              GNU-Emacs                                            ;
;                                                                                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; M-x load-file ~./emacs
;
; C-x C-f /path/to/... .el RET
; M-x byte-compile-file RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Path
;

(add-to-list 'load-path (expand-file-name "/home/etc/users/emacs-site-lisp"))
(add-to-list 'load-path (expand-file-name "/home/etc/users/emacs-site-lisp/flymake-python"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Frame settings
;

;(speedbar t)

; Disable the tool bar
(tool-bar-mode 0)

; Menu for recently opened files
(recentf-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Set Font
;

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(defun initialise_font ()
  (interactive)
  (if window-system
      (progn
;        (setq initial-frame-alist '((top    . 1)
;				    (left   . 1)
;				    (width  . 200)
;				    (height . 200)))
;       (set-default-font "-adobe-courier-medium-r-normal-*-*-140-*-*-m-*-iso8859-15")
;	(set-default-font "-jis-fixed-medium-r-normal--16-110-100-100-c-160-jisx0208.1983-0")
;	(set-foreground-color "wheat")
;	(set-background-color "black")
	(set-foreground-color "black")
	(set-background-color "white")
;       Set the line spacing
	(setq-default line-spacing 3) ; px
;       (setq-default line-spacing .15) ; %
	)))

(initialise_font )  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Key Bindings
;

(global-set-key [delete] 'delete-char) 

(global-set-key [C-up]   'ignore) ; was backward-paragraph
(global-set-key [C-down] 'ignore) ; was forward-paragraph
 
(global-set-key [C-prior] 'ignore) ; was scroll-right
(global-set-key [C-next]  'ignore) ; was scroll-left

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Mule
;

;(set-keyboard-coding-system 'mule-utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Fill-column & Auto fill mode
;

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq fill-column 100)

(add-hook 'TeX-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'outline-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'python-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'rst-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'text-mode-hook '(lambda () (setq fill-column 80)))

(setq column-number-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Indent Tabs Mode
;

; (setq-default 'indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Mode
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; iswitchb
;

(require 'iswitchb) 
(iswitchb-default-keybindings) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Info
;

;(setq Info-directory-list Info-default-directory-list)
;(add-to-list 'Info-directory-list "/media/home/salvaire/local_stow/info")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; dired
;

(add-hook 'dired-load-hook
	  (lambda ()
	    (load "dired-x")
	    ;; Set dired-x global variables here.
	    ))

(add-hook 'dired-mode-hook
	  (lambda ()
	    ;; Set dired-x buffer-local variables here.
	    ;; dired-omit-toggle M-o
	    ; (setq dired-omit-files-p t)
	    (setq dired-omit-files
		  ; do not wish to see `dot' files (files starting with a `.')
		  (concat dired-omit-files "\\|^\\..+$"))
	    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                  
; Aspell                                           
;                                                  

(setq-default ispell-program-name "aspell")

(setq-default flyspell-default-dictionary "british")
(setq-default ispell-extra-args nil)

(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." t)  
(autoload 'tex-mode-flyspell-verify "flyspell" "" t)                 
(add-hook 'LaTeX-mode-hook 'flyspell-mode)                           

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
;
; hunspell
;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Mutt
;

(autoload 'muttrc-mode "muttrc-mode.el" "Major mode to edit muttrc files" t)
(setq auto-mode-alist
      (append '(("muttrc\\'" . muttrc-mode))
	      auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; TeX
;

;
; AUC-TeX
;
(require 'tex-site)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;(if window-system
;    (require 'font-latex))

(add-hook 'LaTeX-mode-hook 'turn-on-reftex) ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Autoconf
;

(autoload 'autoconf-mode "autoconf-mode"
  "Major mode for editing autoconf files." t)
(setq auto-mode-alist
      (cons '("\\.ac\\'\\|configure\\.in\\'" . autoconf-mode)
	    auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; mode doxygen
;

;(require 'doxymacs)
;(add-hook 'c-mode-common-hook 'doxymacs-mode)
;(defun my-doxymacs-font-lock-hook ()
;  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;      (doxymacs-font-lock)))
;(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; mode gtags
;

(autoload 'gtags-mode "gtags" "" t)
(setq c-mode-hook
      '(lambda ()
	 (gtags-mode 1)
	 ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Web
;

(autoload 'css-mode "css-mode")
(setq auto-mode-alist       
      (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Perl
;

;(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
;(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
;(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
;(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
;(defalias 'perl-mode 'cperl-mode)
;(setq cperl-invalid-face (quote off)) ; pour ____
;(setq cperl-electric-keywords t)
;(setq cperl-hairy t)
;(setq cperl-auto-newline t)
;(setq cperl-highlight-variables-indiscriminately t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Python
;

; (setq auto-mode-alist
;       (cons '("\\.py$" . python-mode) auto-mode-alist))
; (setq interpreter-mode-alist
;       (cons '("python" . python-mode)
;             interpreter-mode-alist))
; 
; (autoload 'python-mode "python-mode" "Python editing mode." t)

;(when (load "flymake" t)
;  (defun flymake-pylint-init ()
;    (let* ((temp-file (flymake-init-create-temp-buffer-copy
;		       'flymake-create-temp-inplace))
;           (local-file (file-relative-name
;                        temp-file
;                        (file-name-directory buffer-file-name))))
;      (list "epylint" (list local-file))))
;  
;  (add-to-list 'flymake-allowed-file-name-masks
;               '("\\.py\\'" flymake-pylint-init)))

(when (load "flymake" t)
  (defun flymake-pylint-init (&optional trigger-type)
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-with-folder-structure))
	   (local-file (file-relative-name
			temp-file
			(file-name-directory buffer-file-name)))
	   (options (when trigger-type (list "--trigger-type" trigger-type))))
      (list "/home/etc/users/emacs-site-lisp/flymake-python/pyflymake.py"
	    (append options (list local-file)))))

  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.py\\'" flymake-pylint-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; C
;

(add-hook 'c-mode-common-hook
	  '(lambda () (c-set-style "gnu")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; PHP
;

; (require 'php-mode)
; (add-hook 'php-mode-user-hook 'turn-on-font-lock)
; (add-hook 'php-mode-user-hook
; 	  '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Puppet
;

(autoload 'puppet-mode "puppet-mode.el" "Puppet editing mode." t)

(setq auto-mode-alist       
      (cons '("\\.pp$" . puppet-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Graphviz Dot
;

(load-file "/home/etc/users/emacs-site-lisp/graphviz-dot-mode.el") 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; GLSL
;

(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Yaml
;

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Unlike python-mode, this mode follows the Emacs convention of not
;; binding the ENTER key to `newline-and-indent'.  To get this
;; behavior, add the key definition to `yaml-mode-hook':
;;
;;    (add-hook 'yaml-mode-hook
;;     '(lambda ()
;;        (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Load
;

(load "/home/etc/users/emacs-site-lisp/emacs-functions.el" t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Custum Settings
;

(setq custom-file "/home/etc/users/gnu-emacs-custom")
(load "/home/etc/users/gnu-emacs-custom" t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Server
;

(server-start) ; Starts server for (among others) emacsclient

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; End
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

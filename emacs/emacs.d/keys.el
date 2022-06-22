;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Key Bindings
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Commands.html
;;
;; to redefines C-x C-\ to move down a line
;;   (global-set-key (kbd "C-x C-\\") 'next-line)
;;   (global-set-key [?\C-x ?\C-\\] 'next-line)
;;   (global-set-key [(control ?x) (control ?\\)] 'next-line)
;;
;; C-   Control
;; M-   Alt- or Esc
;; s-   Super = Windows Key
;; H-   Hyper
;;
;; X11
;;   Fn      : 151 XF86WakeUp
;;   Ctrl    : 37
;;   Windows : 133 RWIN : Mode_switch / Multi_key
;;   Alt L   : 64
;;   Hyper_L Super_L
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; M-x speedbar

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ivy
;;

;; (global-set-key "\C-s" 'swiper)
(global-set-key "\C-s" 'swiper-isearch) ; A swiper that’s not line-based
;; ivy-yank-word that behaves similarly to C-w of isearch.
;; It's not bound by default, since I want ivy's minibuffer to behave more like a normal buffer, so C-w should call kill-region.
;; Additionally, there's an option to search for symbol-at-point with C-s M-n.
;;   This one's different from ivy-yank-word, since it inputs symbol-at-point: you can be in the
;;   middle or behind the symbol, not just at the front.
;; binded to M-j
;; (define-key ivy-minibuffer-map (kbd "C-w") 'ivy-yank-word)

(global-set-key (kbd "C-c C-r") 'ivy-resume)
;; (global-set-key (kbd "<f6>") 'ivy-resume)

(global-set-key (kbd "M-x") 'counsel-M-x)

(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)

(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

;; (global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
;; (global-set-key (kbd "C-c k") 'counsel-ag)
;; (global-set-key (kbd "C-x l") 'counsel-locate)
;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;; (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Treeemacs
;;

;; cf. treeemacs-settings.el

;; ("M-0"       . treemacs-select-window)
;; ("C-x t 1"   . treemacs-delete-other-windows)
;; ("C-x t t"   . treemacs)  ;; open Treemacs frame
;; ("C-x t d"   . treemacs-select-directory)
;; ("C-x t B"   . treemacs-bookmark)
;; ("C-x t C-t" . treemacs-find-file)
;; ("C-x t M-t" . treemacs-find-tag)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Projectile
;;

; cf. programming.el
; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Edition
;;

(global-set-key [delete] 'delete-char)

(global-unset-key [C-up]) ; was backward-paragraph
(global-unset-key [C-down]) ; was forward-paragraph

(global-unset-key [C-prior]) ; was scroll-right
(global-unset-key [C-next]) ; was scroll-left

;; Use `C-c m' interactively to highlight with face `column-marker-1'.
;; (global-set-key [?\C-c ?m] 'column-marker-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Dired
;;

(global-set-key (kbd "M-o") 'dired-omit-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Spelling
;;

(global-set-key "\C-clc" 'languagetool-check)
(global-set-key "\C-cld" 'languagetool-clear-buffer)
(global-set-key "\C-clp" 'languagetool-correct-at-point)
(global-set-key "\C-clb" 'languagetool-correct-buffer)
(global-set-key "\C-cll" 'languagetool-set-language)

;; (global-set-key "\C-ct" 'google-translate-at-point)
(global-set-key "\C-ct" 'google-translate-smooth-translate)
(global-set-key "\C-cT" 'google-translate-query-translate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Completion / Helm
;;

;; (global-set-key (kbd "M-x") 'helm-M-x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Programming
;;

(global-set-key (kbd "C-x g")   'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(global-set-key [C-M-tab] 'clang-format-region)

(global-set-key [f12] 'custom-indent)
(global-set-key [f11] 'custom-doxygen)

(global-set-key (kbd "M-~") 'string-inflection-underscore)

(global-set-key (kbd "C-$") 'comment-or-uncomment-region)

(global-set-key [f6] 'insert-long-rule)
(global-set-key [f7] 'insert-short-rule)

;(global-set-key [M-delete] 'kill-word)
;(global-set-key [M-backspace] 'backward-kill-word)
(global-set-key [C-delete] 'py-kill-word)
(global-set-key [C-backspace] 'py-backward-kill-word)

(global-set-key "\M-&" 'insert-and)
(global-set-key "\M-|" 'insert-or)

;; (global-set-key (kbd "C-n") company-select-next-or-abort)
;; (global-set-key (kbd "M-n") company-select-next)
;; (global-set-key (kbd "C-p") company-select-previous-or-abort)
;; (global-set-key (kbd "M-p") company-select-previous)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Mule
;;

;; (set-keyboard-coding-system 'mule-utf-8)

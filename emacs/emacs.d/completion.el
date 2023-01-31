;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Completion and Buffer Cycling
;;  in Emacs UI
;;  not for the text in a buffer, see company text completion framework
;;
;;   Fixme: icomplete / icicles / helm
;;
;;  https://github.com/alphapapa/burly.el — Save and restore frames and windows with their buffers in Emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ivy
;;  Ivy is a generic completion mechanism for Emacs
;;  https://github.com/abo-abo/swiper
;;  Manual https://oremacs.com/swiper

(use-package ivy
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (search-default-mode #'char-fold-to-regexp)
  (magit-completing-read-function 'ivy-completing-read)

  :config
  ;; Enable Ivy completion everywhere
  (ivy-mode 1)
)

;; Search for the word at the current cursor position. By default, we can do this by starting a
;; normal isearch with C-s and then hitting C-w to search for the current word. Keep hitting C-w to
;; add subsequent words to the search.
;; C-w is binded to kill-region
;;
;; https://github.com/abo-abo/swiper/issues/133
;;
;; https://pragmaticemacs.wordpress.com/2016/08/23/search-or-swipe-for-the-current-word/
;; version of ivy-yank-word to yank from start of word
;; (defun bjm/ivy-yank-whole-wordivy-yank-whole-word ()
;;   "Pull next word from buffer into search string."
;;   (interactive)
;;   (let (amend)
;;     (with-ivy-window
;;       ;;move to last word boundary
;;       (re-search-backward "\\b")
;;       (let ((pt (point))
;;             (le (line-end-position)))
;;         (forward-word 1)
;;         (if (> (point) le)
;;             (goto-char pt)
;;           (setq amend (buffer-substring-no-properties pt (point))))))
;;     (when amend
;;       (insert (replace-regexp-in-string "  +" " " amend)))))
;; (define-key ivy-minibuffer-map (kbd "M-j") 'bjm/ivy-yank-whole-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; which-key — displays available keybindings in popup
;;  htpps://github.com/justbur/emacs-which-key

(use-package which-key
    :config
    (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helm - Emacs incremental completion and selection narrowing framework
;;   https://github.com/emacs-helm/helm

;; (use-package helm-config)
;; (helm-mode 1)

;; (setq default-frame-alist '((vertical-scroll-bars . nil)
;;                             (tool-bar-lines . 0)
;;                             (menu-bar-lines . 0)
;;                             (fullscreen . nil)))
;; (blink-cursor-mode -1)

;; (define-key global-map [remap find-file] 'helm-find-files)
;; (define-key global-map [remap occur] 'helm-occur)
;; (define-key global-map [remap list-buffers] 'helm-buffers-list)
;; (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)

;; (unless (boundp 'completion-in-region-function)
;;   (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
;;   (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; icomplete-mode
;;   (replaces iswitchb-mode)
;;   https://www.emacswiki.org/emacs/IcompleteMode

;; (icomplete-mode 1)

;; (eval-after-load "icomplete" '(progn (require 'icomplete+)))
;; ;; (icomplete-cycling-mode 99)
;; (icompletep-cycling-mode 99)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; icicles — Emacs enhances minibuffer completion
;;   https://www.emacswiki.org/emacs/Icicles
;;
;; Issues:
;;   M-x history
;;   find-file

;; (use-package icicles)
;; (icy-mode 1)

;; (setq icicle-buffer-include-recent-files-nflag t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ido
;;   http://ergoemacs.org/emacs/emacs_buffer_switching.html

;; (use-package ido-mode)

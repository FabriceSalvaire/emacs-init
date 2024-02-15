;;; .el --- ... -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Completion and Buffer Cycling
;;  in Emacs UI
;;  not for the text in a buffer, see company text completion framework
;;
;;  Fixme: icomplete / icicles / helm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ivy
;;  Ivy is a generic completion mechanism for Emacs
;;  https://github.com/abo-abo/swiper
;;  Manual https://oremacs.com/swiper

;; https://github.com/CSRaghunandan/.emacs.d/blob/master/setup-files/setup-ivy.el

;; this a new addition after -> straight
(use-package counsel
  )

(use-package ivy
  ;;s;; :ensure t

  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (search-default-mode #'char-fold-to-regexp)
  (magit-completing-read-function 'ivy-completing-read)

  :config
  ;; M-x ivy-mode
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

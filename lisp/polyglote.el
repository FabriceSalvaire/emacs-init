;;; .el --- ... -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; [polymode/polymode: Framework for Multiple Major Modes in Emacs](https://github.com/polymode/polymode)
;; [Polymode](https://polymode.github.io)
;; [Polymode: Multiple Major Modes and How to Use SQL and Python in one Buffer - Mastering Emacs](https://www.masteringemacs.org/article/polymode-multiple-major-modes-how-to-use-sql-python-in-one-buffer)
;; [polymode/poly-markdown: Polymode for markdown-mode](https://github.com/polymode/poly-markdown)

;; rendering is voodoo, must edit cell to refresh...
;; markdown-mode hides link
;; don't save in hiding mode !!!

;; [EmacsWiki : Multiple Modes](https://www.emacswiki.org/emacs/MultipleModes)

;; [glyph/python-docstring-mode: Emacs minor-mode for editing Python docstrings](https://github.com/glyph/python-docstring-mode)
;; [Edit docstrings with markdown-mode - julia-emacs](https://github.com/JuliaEditorSupport/julia-emacs/issues/50)

;; [Markdown Mode for Emacs](https://jblevins.org/projects/markdown-mode/)

(use-package polymode
  :disabled
  :ensure t
  ;; :mode ("\.py$" . poly-python-sql-mode)
  ;; :mode ("\.py$" . poly-python-md-mode)
  :config
  (setq polymode-prefix-key (kbd "C-c n"))
  (define-hostmode poly-python-hostmode :mode 'python-mode)

  (define-innermode poly-sql-expr-python-innermode
    :mode 'sql-mode
    :head-matcher (rx "r" (= 3 (char "\"'")) (* (any space)))
    :tail-matcher (rx (= 3 (char "\"'")))
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-md-expr-python-innermode
    :mode 'markdown-mode
    :head-matcher (rx "r" (= 3 (char "\"'")) (* (any space)))
    :tail-matcher (rx (= 3 (char "\"'")))
    :head-mode 'host
    :tail-mode 'host)

  (defun poly-python-sql-eval-chunk (beg end msg)
    "Calls out to `sql-send-region' with the polymode chunk region"
    (sql-send-region beg end))

  (define-polymode poly-python-sql-mode
    :hostmode 'poly-python-hostmode
    :innermodes '(poly-sql-expr-python-innermode)
    (setq polymode-eval-region-function #'poly-python-sql-eval-chunk)
    (define-key poly-python-sql-mode-map (kbd "C-c C-c") 'polymode-eval-chunk))

  (define-polymode poly-python-md-mode
    :hostmode 'poly-python-hostmode
    :innermodes '(poly-md-expr-python-innermode)
    ;; (setq polymode-eval-region-function #'poly-python-md-eval-chunk)
    ;;(define-key poly-python-md-mode-map (kbd "C-c C-c") 'polymode-eval-chunk)
    )

  ;; Bug? Fix polymode kill chunk so it works.
  (defun polymode-kill-chunk ()
    "Kill current chunk."
    (interactive)
    (pcase (pm-innermost-span)
      (`(,(or `nil `host) ,beg ,end ,_) (delete-region beg end))
      (`(body ,beg ,_ ,_)
       (goto-char beg)
       (pm--kill-span '(body))
       ;; (pm--kill-span '(head tail))
       ;; (pm--kill-span '(head tail))
       )
      (`(tail ,beg ,end ,_)
       (if (eq beg (point-min))
           (delete-region beg end)
         (goto-char (1- beg))
         (polymode-kill-chunk)))
      (`(head ,_ ,end ,_)
       (goto-char end)
       (polymode-kill-chunk))
      (_ (error "Canoot find chunk to kill")))))

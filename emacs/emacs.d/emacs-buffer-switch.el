;; Fixme: icomplete / icicles / helm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; icomplete-mode (replaces iswitchb-mode)
;; https://www.emacswiki.org/emacs/IcompleteMode

(icomplete-mode 1)

(eval-after-load "icomplete" '(progn (require 'icomplete+)))
;; (icomplete-cycling-mode 99)
(icompletep-cycling-mode 99)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; icicles
;; https://www.emacswiki.org/emacs/Icicles

;; Issues:
;;  M-x history
;;  find-file

;; (require 'icicles)
;; (icy-mode 1)

;; (setq icicle-buffer-include-recent-files-nflag t)

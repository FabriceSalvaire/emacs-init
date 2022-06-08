;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NGinx
;;

(use-package nginx-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Puppet
;;

(if nil
    (progn
      (autoload 'puppet-mode "puppet-mode.el" "Puppet editing mode." t)
      (setq auto-mode-alist
	    (cons '("\\.pp$" . puppet-mode) auto-mode-alist))
      ))

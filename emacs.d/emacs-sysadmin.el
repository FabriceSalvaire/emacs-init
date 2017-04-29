;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Puppet
;

(autoload 'puppet-mode "puppet-mode.el" "Puppet editing mode." t)

(setq auto-mode-alist
      (cons '("\\.pp$" . puppet-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; NGinx
;

(require 'nginx-mode)

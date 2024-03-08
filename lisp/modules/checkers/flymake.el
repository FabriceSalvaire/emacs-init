;;; syntax.el -*- lexical-binding: t; -*-

;;
;;; Flymake
(use-package! flymake
  :when (modulep! +flymake)
  :defer t
  :init
  ;; as flymakes fail silently there is no need to activate it on a per major mode basis
  (add-hook! (prog-mode text-mode) #'flymake-mode)
  :config
  (setq flymake-fringe-indicator-position 'right-fringe))


(use-package! flymake-popon
  :when (modulep! +flymake)
  :hook (flymake-mode . flymake-popon-mode)
  :config
  (setq flymake-popon-method (if (modulep! +childframe)
                                 'posframe
                               'popon)))

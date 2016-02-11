;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Fill-column & Auto fill mode
;

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq fill-column 100)

(add-hook 'TeX-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'c-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'cmake-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'glsl-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'outline-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'python-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'rst-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'sass-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'text-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'web-mode-hook '(lambda () (setq fill-column 100)))
(add-hook 'yaml-mode-hook '(lambda () (setq fill-column 100)))

(setq column-number-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Indent Tabs Mode
;

; (setq-default 'indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Mule
;

;(set-keyboard-coding-system 'mule-utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; End
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

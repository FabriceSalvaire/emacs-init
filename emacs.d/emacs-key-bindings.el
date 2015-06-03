;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Key Bindings
;

(global-set-key [delete] 'delete-char)

(global-unset-key [C-up]) ; was backward-paragraph
(global-unset-key [C-down]) ; was forward-paragraph

(global-unset-key [C-prior]) ; was scroll-right
(global-unset-key [C-next]) ; was scroll-left

(global-set-key (kbd "C-$")  'comment-or-uncomment-region)

(global-set-key (kbd "M-o")  'dired-omit-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; End
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

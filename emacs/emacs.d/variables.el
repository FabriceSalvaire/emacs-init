(setq native-comp-async-report-warnings-errors nil)

;; spam asking y/n for some files...
;; allow remembering risky variables
(defun risky-local-variable-p (sym &optional _ignored) nil)

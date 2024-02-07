(deftheme fabrice-dark
  "Created 2024-02-06.")

;; (custom-set-faces '(default ((t (:family "Noto Sans Mono" :foundry "GOOG" :slant normal :weight normal :height 143 :width normal)))))

;; https://github.com/doomemacs/themes/blob/master/themes/doom-one-theme.el
;; (set-foreground-color "#bbc2cf") ;  hsv(219, 25, 207) "#bfbfbf" "brightwhite"
;; (set-background-color "#282c34") ;  hsv(220, 59,  52)  "black"

;; (require 'color)
;; (color-darken-name "#ffffff" 40)

;; height 113
;; fg "#cccccc" ; "#bbc2cf"
;; bg "#1f1f1f" ; "#282c34"

(custom-theme-set-faces
 'fabrice-dark

 ;; '(default ((t (:family "Source Code Pro" :foundry "ADBO" :width normal :height 113 :weight regular :slant normal :underline nil :overline nil :extend nil :strike-through nil :box nil :inverse-video nil :foreground "#bbc2cf" :background "#282c34" :stipple nil :inherit nil))))

 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight medium :height 120 :width normal))))

 
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight medium :height 120 :width normal :foreground "#cccccc" :background "#1f1f1f" :underline nil :overline nil :extend nil :strike-through nil :box nil :inverse-video nil :stipple nil :inherit nil))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Sans Serif"))))

 '(cursor ((t (:background "#51afef"))))
 '(escape-glyph ((t (:foreground "#46D9FF"))))
 '(homoglyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((t (:foreground "#51afef"))))
 '(highlight ((t (:foreground "#1B2229" :background "#51afef"))))
 '(region ((t (:extend t :background "#42444a"))))
 '(shadow ((t (:foreground "#5B6268"))))
 '(secondary-selection ((t (:extend t :background "#3f444a"))))
 '(trailing-whitespace ((t (:background "#ff6c6b"))))

 '(font-lock-bracket-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-builtin-face ((t (:foreground "#c678dd")))) ; violet
 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:foreground "#6A9955")))) ; vert / gris foncé #5B6268
 '(font-lock-constant-face ((t (:foreground "#a9a1e1")))) ; bleu violet
 '(font-lock-delimiter-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-doc-face ((t (:foreground "#ce9178" :inherit (font-lock-comment-face))))) ; "#83898d" rose sombre
 '(font-lock-doc-markup-face ((t (:inherit (font-lock-constant-face)))))
 '(font-lock-escape-face ((t (:inherit (font-lock-regexp-grouping-backslash)))))
 '(font-lock-function-call-face ((t (:inherit (font-lock-function-name-face)))))
 '(font-lock-function-name-face ((t (:foreground "#c678dd")))) ; violet
 '(font-lock-keyword-face ((t (:foreground "#51afef")))) ; bleu
 '(font-lock-negation-char-face ((t (:foreground "#51afef" :inherit (bold)))))
 '(font-lock-number-face ((t (:foreground "#da8548")))) ; orange
 '(font-lock-misc-punctuation-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-operator-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:foreground "#51afef" :inherit (bold)))))
 '(font-lock-property-name-face ((t (:inherit (font-lock-variable-name-face)))))
 '(font-lock-property-use-face ((t (:inherit (font-lock-property-name-face)))))
 '(font-lock-punctuation-face ((t nil)))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#51afef" :inherit (bold))))) ; bleu
 '(font-lock-regexp-grouping-construct ((t (:foreground "#51afef" :inherit (bold))))) ; bleu
 '(font-lock-string-face ((t (:foreground "#98be65")))) ; vert
 '(font-lock-type-face ((t (:foreground "#ecbe7b")))) ; jaune
 '(font-lock-variable-name-face ((t (:foreground "#dcaeea")))) ; violet pastel
 '(font-lock-variable-use-face ((t (:inherit (font-lock-variable-name-face)))))
 '(font-lock-warning-face ((t (:inherit (warning)))))

 '(button ((t (:inherit (link)))))
 '(link ((t (:weight bold :underline (:color foreground-color :style line :position nil) :foreground "#51afef"))))
 '(link-visited ((default (:inherit (link))) (((class color) (background light)) (:foreground "magenta4")) (((class color) (background dark)) (:foreground "violet"))))
 '(fringe ((t (:foreground "#3f444a" :inherit (default)))))
 '(header-line ((t (:inherit (mode-line)))))
 '(tooltip ((t (:foreground "#bbc2cf" :background "#21242b"))))

 '(mode-line ((t (:box nil :foreground "#bbc2cf" :background "#1d2026"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:foreground "#51afef"))))
 '(mode-line-highlight ((t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:box nil :foreground "#5B6268" :background "#21242b")))) ; gris

 '(isearch ((t (:weight bold :inherit (lazy-highlight)))))
 '(isearch-fail ((t (:weight bold :foreground "#1B2229" :background "#ff6c6b"))))

 '(lazy-highlight ((t (:weight bold :foreground "#DFDFDF" :background "#387aa7"))))
 '(match ((t (:weight bold :foreground "#98be65" :background "#1B2229"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))

 '(whitespace-empty ((t (:background "#808080" :foreground "#ee0000"))))
 '(whitespace-tab ((t (:inherit (whitespace-empty)))))
 '(whitespace-trailing ((t (:inherit (whitespace-empty)))))

 ;; '(highlight-indent-guides-character-face ((t (:foreground "gainsboro"))))
 ;; '(highlight-indent-guides-stack-character-face ((t (:inherit (highlight-indent-guides-character-face)))))
 ;; '(highlight-indent-guides-top-character-face ((t (:inherit (highlight-indent-guides-character-face)))))
 )

(provide-theme 'fabrice-dark)

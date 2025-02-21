(deftheme tuxikus-basic
  "Nice theme")

(custom-theme-set-faces
 'tuxikus-basic
 '(default ((t (:family "Iosevka Nerd Font" :width normal :height 151 :weight regular :slant normal :underline nil :overline nil :extend nil :strike-through nil :box nil :inverse-video nil :foreground "#00ff00" :background "#000000" :stipple nil :inherit nil))))
 '(cursor ((t (:background "#ffffff"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Sans Serif"))))
 '(escape-glyph ((t (:foreground "#e7a59a"))))
 '(homoglyph ((t (:foreground "#f5aa80"))))
 '(minibuffer-prompt ((t (:inherit (modus-themes-prompt)))))
 '(highlight ((t (:foreground "#00ff00" :background "#00415e"))))
 '(region ((t (:extend t :foreground "#ffffff" :background "#3c3c3c"))))
 '(shadow ((t (:foreground "#a8a8a8"))))
 '(secondary-selection ((t (:extend t :inherit (modus-themes-special-cold)))))
 '(trailing-whitespace ((t (:background "#a4202a"))))
 '(font-lock-bracket-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-builtin-face ((t (:foreground "#f78fe7" :inherit (modus-themes-bold)))))
 '(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:foreground "#a8a8a8" :inherit (modus-themes-slant)))))
 '(font-lock-constant-face ((t (:foreground "#00bcff"))))
 '(font-lock-delimiter-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-doc-face ((t (:foreground "#b0d6f5" :inherit (modus-themes-slant)))))
 '(font-lock-doc-markup-face ((t (:inherit (font-lock-constant-face)))))
 '(font-lock-escape-face ((t (:inherit (font-lock-regexp-grouping-backslash)))))
 '(font-lock-function-call-face ((t (:inherit (font-lock-function-name-face)))))
 '(font-lock-function-name-face ((t (:foreground "#feacd0"))))
 '(font-lock-keyword-face ((t (:foreground "#b6a0ff" :inherit (modus-themes-bold)))))
 '(font-lock-negation-char-face ((t (:foreground "#d0bc00" :inherit (modus-themes-bold)))))
 '(font-lock-number-face ((t nil)))
 '(font-lock-misc-punctuation-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-operator-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:foreground "#ff9077"))))
 '(font-lock-property-name-face ((t (:inherit (font-lock-variable-name-face)))))
 '(font-lock-property-use-face ((t (:inherit (font-lock-property-name-face)))))
 '(font-lock-punctuation-face ((t nil)))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#abab00" :inherit (modus-themes-bold)))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "#e7a59a" :inherit (modus-themes-bold)))))
 '(font-lock-string-face ((t (:foreground "#79a8ff"))))
 '(font-lock-type-face ((t (:foreground "#6ae4b9" :inherit (modus-themes-bold)))))
 '(font-lock-variable-name-face ((t (:foreground "#00d3d0"))))
 '(font-lock-variable-use-face ((t (:inherit (font-lock-variable-name-face)))))
 '(font-lock-warning-face ((t (:foreground "#d0bc00" :inherit (modus-themes-bold)))))
 '(button ((t (:underline (:color foreground-color :style line :position nil) :foreground "#00bcff"))))
 '(link ((t (:inherit (button)))))
 '(link-visited ((t (:underline (:color foreground-color :style line :position nil) :foreground "#b6a0ff" :inherit (button)))))
 '(fringe ((t (:foreground "#ffffff" :background "#000000"))))
 '(header-line ((t (:box (:line-width (4 . 4) :color "#212121" :style nil) :foreground "#dddddd" :background "#212121" :inherit (modus-themes-ui-variable-pitch)))))
 '(tooltip ((t (:foreground "#ffffff" :background "#203448"))))
 '(mode-line ((t (:box (:line-width (6 . 6) :color "#2a2a66" :style nil) :foreground "#ffffff" :background "#2a2a66" :inherit (modus-themes-ui-variable-pitch)))))
 '(mode-line-buffer-id ((t (:inherit (bold)))))
 '(mode-line-emphasis ((t (:foreground "#d5b1ff" :inherit (bold)))))
 '(mode-line-highlight ((t (:box (:line-width (1 . 1) :color "#ffffff" :style nil) :inherit (highlight)))))
 '(mode-line-inactive ((t (:box (:line-width (6 . 6) :color "#1e1e1e" :style nil) :foreground "#bfc0c4" :background "#1e1e1e" :inherit (modus-themes-ui-variable-pitch)))))
 '(isearch ((t (:inherit (modus-themes-search-success)))))
 '(isearch-fail ((t (:inherit (modus-themes-refine-red)))))
 '(lazy-highlight ((t (:inherit (modus-themes-search-success-lazy)))))
 '(match ((t (:inherit (modus-themes-special-calm)))))
 '(next-error ((t (:extend t :inherit (modus-themes-subtle-red)))))
 '(query-replace ((t (:inherit (modus-themes-intense-red))))))

(provide-theme 'tuxikus-basic)

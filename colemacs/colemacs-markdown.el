(use-package markdown-mode
  :custom
  (markdown-command "pandoc --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash --self-contained --css /Users/broeg1/dotfiles/github-pandoc.css")
  (markdown-header-scaling t)
  (markdown-header-scaling-values '(1.2 1.15 1.1 1.05 1 1))
  (markdown-list-item-bullets '("►" "●" "•" "○" "◆" "◇" "-"))
  (markdown-list-indent-width 2)
  :config
  (set-face-attribute 'markdown-header-face-1 nil :foreground "#fff0bd")
  (set-face-attribute 'markdown-header-face-2 nil :foreground "#5c7ae0")
  (set-face-attribute 'markdown-header-face-3 nil :foreground "#23a39a")
  (set-face-attribute 'markdown-header-face-4 nil :foreground "#81c87c")
  (set-face-attribute 'markdown-header-delimiter-face nil :foreground "#474747")
  (set-face-attribute 'markdown-italic-face nil :foreground "#fff0bd")
  (set-face-attribute 'markdown-html-attr-name-face nil :foreground "#9f7161")
  (set-face-attribute 'markdown-html-attr-value-face nil :foreground "#c08b4c")
  (set-face-attribute 'markdown-list-face nil :foreground "#6a6a6a")
  )


(use-package grip-mode)
(use-package quarto-mode)

(cole/local-leader-keys markdown-mode-map
  "P" '(grip-mode :which-key "preview (grip)")
  "q" '((lambda () (interactive) (quarto-preview)):which-key "quarto preview")
  "r" '(markdown-export :which-key "render")
  "o" '(markdown-follow-thing-at-point :which-key "open thing at point")
  "i" '(:ignore t :which-key "insert")
  "il" '(markdown-insert-link :which-key "link")
  "if" '(markdown-insert-footnote :which-key "footnote")
  "iF" '(markdown-insert-foldable-block :which-key "foldable block")
  "ic" '(markdown-insert-code :which-key "code")
  "it" '(markdown-insert-table :which-key "table")
  "ii" '(markdown-insert-image :which-key "image")
  "t" '(:ignore t :which-key "toggle")
  "tm" '(markdown-toggle-math :which-key "math")
  "tf" '(markdown-toggle-fontify-code-blocks-natively :which-key "fontify code blocks") ;; TODO make this on by default
  "ti" '(markdown-toggle-inline-images :which-key "inline images")
  "tu" '(markdown-toggle-url-hiding :which-key "url hiding")
  "tm" '(markdown-toggle-markup-hiding :which-key "markup hiding")
  "x" '(:ignore t :which-key "text")
  "xi" '(markdown-insert-italic :which-key "italic")
  "xb" '(markdown-insert-bold :which-key "bold")
  )

(use-package mermaid-mode
  :custom
  (mermaid-output-format ".png")
  (mermaid-flags "--scale 4 --theme forest --pdfFit"))

(cole/local-leader-keys mermaid-mode-map
  "r" '(mermaid-compile :which-key "render file to image")
  "h" '(mermaid-open-doc :which-key "help open doc")
  "o" '(mermaid-open-browser :which-key "edit in online editor"))

(provide 'colemacs-markdown)

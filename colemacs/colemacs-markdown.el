(cole/local-leader-keys markdown-mode-map
  "P" '(grip-mode :which-key "preview (grip)")
  ;; "q" '((lambda () (interactive) (async-shell-command '(quarto preview))) :which-key "quarto preview")
  "p" '(markdown-export :which-key "preview (mdopen)")
  "o" '(markdown-follow-thing-at-point :which-key "open thing at point")
  "i" '(:ignore t :which-key "insert")
  "ic" '(cole/ess-insert-r-code-chunk :which-key "chunk")
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

(defun cole/ess-insert-r-code-chunk ()
  "Insert an R Markdown code chunk."
  (interactive)
  (insert "```{r}\n")
  (save-excursion
    (insert "\n")
    (insert "```\n")))


(use-package markdown-mode
  :custom
  (markdown-command "pandoc --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash --embed-resources --standalone --css ~/dotfiles/resources/github-pandoc.css")
  (markdown-header-scaling t)
  (markdown-header-scaling-values '(1.2 1.15 1.1 1.05 1 1))
  (markdown-list-item-bullets '("►" "●" "•" "○" "◆" "◇" "-"))
  (markdown-list-indent-width 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.rmd" . markdown-mode))
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

;; (use-package mdopen-mode
;;   :load-path "~/.cargo/bin/mdopen"
;;   :hook (markdown-mode . mdopen-mode)
;; )

(use-package grip-mode
  :custom
  (grip-update-after-change nil)
  (grip-github-user "cole-brokamp")
  (grip-github-password (exec-path-from-shell-copy-env "GRIP_GH_PAT"))
  )

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  )

(cole/leader-keys
  "S" '(:ignore t :which-key "spelling")
  "Ss" '(flyspell-mode :which-key "toggle spelling mode")
  "Sp" '(flyspell-prog-mode :which-key "enable spelling prog mode")
  "Sb" '(flyspell-buffer :which-key "spell check buffer")
  "Sc" '(flyspell-correct-wrapper :which-key "correct word"))

(use-package flyspell-correct
  :after flyspell)

(use-package flyspell-correct-ivy
  :after flyspell-correct
  :config
  (add-hook 'flyspell-prog-mode-hook (lambda () (message "Flyspell prog-mode enabled in current buffer"))))


(provide 'colemacs-markdown)

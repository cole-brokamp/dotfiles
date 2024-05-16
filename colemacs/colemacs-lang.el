(cole/leader-keys
  "C" '(:ignore t :which-key "checking code")
  "Cc" '(flymake-mode :which-key "checking code")
  )

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (
         (R-mode . lsp)
	 (dockerfile-mode . lsp)
	 (rust-mode . lsp)
	 (html-mode . lsp)
	 (json-ts-mode . lsp)
	 (yaml-mode . lsp)
	 (bash-ts-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-headerline-breadcrumb-mode 'symbols)
  (lsp-toggle-symbol-highlight
  :commands lsp)

(cole/leader-keys
  "L" '(:ignore t :which-key "language server protocol")
  "L!" '(:ignore t :which-key "don't forget about C-c l ...")
  "L=" '(:ignore t :which-key "format")
  "L==" '(lsp-format-buffer :which-key "format buffer")
  "L=r" '(lsp-format-region :which-key "format region")
  "LF" '(:ignore t :which-key "folder")
  "LFa" '(lsp-workspace-folders-add :which-key "add folder")
  "LFr" '(lsp-ivy-workspace-folders-remove :which-key "remove workspace folder")
  "LFu" '(lsp-workspace-blocklist-remove :which-key "unblock workspace folder")
  "Lf" '(:ignore t :which-key "find")
  "Lfd" '(lsp-find-definition :which-key "find definition")
  "Lfr" '(lsp-find-references :which-key "find references")
  "Lfs" '(lsp-ivy-workspace-symbol :which-key "find workspace symbol")
  "Lh" '(lsp-ui-doc-glance :which-key "glance help on thing at point")
  "LH" '(lsp-describe-thing-at-point :which-key "help on thing at point")
  )

(use-package lsp-ui
  :custom
  (lsp-ui-doc-enable 't)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-use-webkit nil)
  (lsp-ui-doc-frame-mode nil)
  :commands lsp-ui-mode)
(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package yaml-mode
    :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
           ("Procfile\\'" . yaml-mode)))

(use-package dockerfile-mode)

(use-package csv-mode)

(use-package rust-mode)

(use-package doc-view
  :custom
  (doc-view-resolution '300)
)

(cole/local-leader-keys doc-view-mode-map
  "W" '(doc-view-fit-width-to-window :which-key "fit width to window")
  "H" '(doc-view-fit-height-to-window :which-key "fit height to window")
  "f" '(doc-view-presentation-mode :which-key "full screen")
  "F" '(doc-view-presentation-exit :which-key "exit full screen")
  "j" '(doc-view-next-line-or-next-page :which-key "scroll down")
  "k" '(doc-view-previous-line-or-previous-page :which-key "scroll down")
  )

(provide 'colemacs-lang)

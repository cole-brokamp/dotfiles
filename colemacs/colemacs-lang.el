(cole/leader-keys
  "C" '(:ignore t :which-key "checking code")
  "Cc" '(flymake-mode :which-key "checking code")
  )

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

(provide 'colemacs-lang)

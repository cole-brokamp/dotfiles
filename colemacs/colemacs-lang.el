(use-package yaml-mode
    :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
           ("Procfile\\'" . yaml-mode)))

(use-package dockerfile-mode)

(use-package csv-mode)

(use-package rust-mode)

(use-package just-mode)
(use-package justl
  :custom
  (justl-executable "/opt/homebrew/bin/just")
  :hook
  (justl-mode . turn-off-evil-mode)
)

(cole/local-leader-keys justl-mode-map
  "g" '(justl-mode :which-key "refresh")
  "o" '(justl-go-to-recipe :which-key "open recipe")
  "e" '(justl-exec-recipe :which-key "exec")
  "E" '(justl-exec-eshell :which-key "exec with eshell")
  "w" '(justl--exec-recipe-with-args :which-key "exec with args")
  "W" '(justl-no-exec-eshell :which-key "open eshell with args")
  "h" '(justl-help-popup :which-key "help")
  )

(provide 'colemacs-lang)

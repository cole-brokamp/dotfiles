
(provide 'colemacs-lang)

(use-package yaml-mode
    :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
           ("Procfile\\'" . yaml-mode)))

(use-package dockerfile-mode)

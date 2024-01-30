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

(provide 'colemacs-spelling)

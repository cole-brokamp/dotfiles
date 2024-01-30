(cole/leader-keys
  "q" '(:ignore t :which-key "quit")
  "qf" '(delete-frame :which-key "kill frame")
  "qq" '(save-buffers-kill-emacs :which-key "quit")
  "qr" '(restart-emacs :which-key "restart")
  "qQ" '(restart-emacs '("no-desktop") :which-key "restart without saving desktop file")
  "qN" '(restart-emacs-start-new-emacs :which-key "start new emacs")
)

(use-package restart-emacs)

(provide 'colemacs-quit)

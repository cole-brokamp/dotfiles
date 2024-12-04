(use-package general
  :init
  (setq general-override-states '(insert
				  emacs
				  hybrid
				  normal
				  visual
				  motion
				  operator
				  replace))
  :config
  (general-create-definer cole/leader-keys
    :states '(normal insert visual motion)
    :prefix "SPC"
    :keymaps 'override
    :non-normal-prefix "C-SPC")
  (general-create-definer cole/local-leader-keys
    :states '(normal insert visual motion)
    :prefix ","
    :keymaps 'override
    :non-normal-prefix "C-,")
  )

(cole/leader-keys
  "SPC" '(counsel-M-x :which-key "M-x")
  "m" '(counsel-evil-marks :which-key "marks")
  "r" '(counsel-register :which-key "registers")
  )

(cole/leader-keys
  "h" '(:ignore t :which-key "help")
  "h." 'display-local-help
  "hF" 'counsel-faces
  "hb" '(general-describe-keybindings :which-key "key bindings")
  "hc" 'describe-command
  "hf" 'describe-function
  "hh" 'help-for-help
  "hi" 'info
  "hk" 'describe-key
  "hm" 'describe-mode
  "hp" 'describe-package
  "hr" '(repeat-complex-command :which-key "repeat complex command")
  "hv" 'describe-variable
  )

(cole/leader-keys
  "i" '(:ignore t :which-key "insert")
  "io" '(newline-and-indent :which-key "open line")
  )

(cole/leader-keys
  "o" '(:ignore t :which-key "open")
  "oo" '(org-open-at-point-global :which-key "open thing at point")
  "ou" '(browse-url-at-point :which-key "open url at point")
  "ox" '(xwidget-webkit-browse-url :which-key "open url in xwidget webkit")
  )

(cole/leader-keys
  "z" '(:ignore t :which-key "fold")
  "zC" '(evil-close-folds :which-key "close all folds")
  "zO" '(evil-open-folds :which-key "open all folds")
  "zc" '(evil-close-fold :which-key "close fold")
  "zo" '(evil-open-fold-rec :which-key "open fold")
  "zz" '(evil-toggle-fold :which-key "toggle fold")
  )

(cole/leader-keys
  "q" '(:ignore t :which-key "quit")
  "qf" '(delete-frame :which-key "kill frame")
  "qq" '(save-buffers-kill-emacs :which-key "quit")
  "qr" '(restart-emacs :which-key "restart")
  "qN" '(restart-emacs-start-new-emacs :which-key "start new emacs")
)
(use-package restart-emacs)

(provide 'colemacs-general)

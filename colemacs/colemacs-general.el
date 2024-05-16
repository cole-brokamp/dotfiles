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
  )

(cole/leader-keys
  "#" '(:ignore :which-key "server edit")
  "##" '(server-edit :which-key "done")
  "#a" '(server-edit :which-key "abort")
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
  "ic" '(counsel-colors-web :which-key "color (web)")
  "iC" '(counsel-colors-emacs :which-key "color (emacs)")
  "ie" '(emoji-insert :which-key "insert emoji")
  "ij" '(evil-collection-unimpaired-insert-newline-below :which-key "insert line below")
  "ik" '(evil-collection-unimpaired-insert-newline-above :which-key "insert line above")
  "iJ" '(evil-collection-unimpaired-paste-below :which-key "paste line below")
  "iK" '(evil-collection-unimpaired-paste-above :which-key "paste line above")
  "io" '(newline-and-indent :which-key "open line")
  )

(cole/leader-keys
  "m" '(counsel-evil-marks :which-key "marks")
  )

(cole/leader-keys
  "o" '(:ignore t :which-key "open")
  "oo" '(org-open-at-point-global :which-key "open thing at point")
  "ou" '(browse-url-at-point :which-key "open url at point")
  "ox" '(xwidget-webkit-browse-url :which-key "open url in xwidget webkit")
  )

(cole/leader-keys
  "r" '(:ignore t :which-key "registers")
  "rf" '(frameset-to-register :which-key "frames save")
  "ri" '(insert-register :which-key "insert text")
  "rj" '(jump-to-register :which-key "jump to")
  "rv" '(counsel-register :which-key "view")
  "rw" '(window-configuration-to-register :which-key "windows save")
  "ry" '(copy-to-register :which-key "yank text")
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
  ;; "qQ" '(restart-emacs '("no-desktop") :which-key "restart without saving desktop file")
  "qN" '(restart-emacs-start-new-emacs :which-key "start new emacs")
)
(use-package restart-emacs)

(provide 'colemacs-general)

(cole/leader-keys
  "b" '(:ignore t :which-key "buffers")
  "bM" '(buf-move :which-key "move buffer")
  "bR" '(font-lock-update :which-key "reload font locks")
  "bb" '(counsel-switch-buffer :which-key "switch to buffer")
  "bB" '(projectile-ibuffer :which-key "ibuffer in project")
  "bd" '(kill-current-buffer :which-key "delete buffer")
  "bf" '(reveal-in-osx-finder :which-key "show buffer in finder")
  "bi" '(ibuffer :which-key "ibuffer")
  "bm" '(cole/switch-to-messages-buffer :which-key "messages buffer")
  "bn" '(cole/buffer-file-name :which-key "copy buffer filename")
  "br" '(revert-buffer :which-key "reload from disk")
  "bs" '(cole/switch-to-scratch-buffer :which-key "scratch buffer")
  )

(cole/leader-keys
  "f" '(:ignore t :which-key "files")
  "fD" '(delete-file :which-key "delete file")
  "fS" '(evil-write-all :which-key "save all")
  "ff" '(counsel-find-file :which-key "find file")
  "fs" '(save-buffer :which-key "save")
  )

(provide 'colemacs-files-buffers)

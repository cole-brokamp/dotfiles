(cole/leader-keys
  "=" '(format-all-buffer :which-key "format buffer")
  "TAB" '(evil-switch-to-windows-last-buffer :which-key "last buffer")
  "b" '(:ignore t :which-key "buffers")
  "bm" '(buf-move :which-key "move buffer")
  "bR" '(font-lock-update :which-key "reload font locks")
  "bb" '(counsel-switch-buffer :which-key "switch to buffer")
  "bB" '(projectile-ibuffer :which-key "ibuffer in project")
  "bd" '(kill-current-buffer :which-key "delete buffer")
  "bf" '(reveal-in-osx-finder :which-key "show buffer in finder")
  "bi" '(ibuffer :which-key "ibuffer")
  "br" '(revert-buffer :which-key "reload from disk")
  "bs" '(scratch-buffer :which-key "scratch buffer")
  "bX" '(desktop-clear :which-key "close ALL the buffers")
  )

(use-package buffer-move)
; TODO create hydra for buffer-move
(use-package reveal-in-osx-finder)
(use-package format-all)

(cole/leader-keys
  "f" '(:ignore t :which-key "files")
  "fD" '(delete-file :which-key "delete file")
  "fS" '(evil-write-all :which-key "save all")
  "ff" '(counsel-find-file :which-key "find file")
  "fs" '(save-buffer :which-key "save")
  "fr" '(counsel-recentf :which-key "recent file")
  "f!" '(recover-this-file :which-key "recover this file from autosave")
  "fR" '(rename-file :which-key "rename file")
  )

(provide 'colemacs-files-buffers)

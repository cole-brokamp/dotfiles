(cole/leader-keys
  "c" '(:ignore t :which-key "compile")
  "cc" '(projectile-compile-project :which-key "compile")
  "cd" '(cole/show-hide-compilation-window :which-key "show/hide compilation window")
  "ck" '(kill-compilation :which-key "kill compilation")
  "J" '(justl :which-key "just")
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

(use-package just-mode)
(use-package justl
  :custom
  (justl-executable "/opt/homebrew/bin/just")
  ;; :hook
  ;; (justl-mode . turn-off-evil-mode)
)

(defun cole/switch-to-compilation-buffer ()
  "Go to last compilation buffer."
  (interactive)
  (if compilation-last-buffer
      (pop-to-buffer compilation-last-buffer)
    (user-error "There is no compilation buffer?")))

(defun cole/show-hide-compilation-window ()
  "Show/Hide the window containing the compilation buffer."
  (interactive)
  (when-let ((buffer compilation-last-buffer))
    (if (get-buffer-window buffer 'visible)
        (delete-windows-on buffer)
      (cole/switch-to-compilation-buffer))))

(provide 'colemacs-compile)

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package vterm
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-buffer-name-string "vterm %s")
  (vterm-always-compile-module t)
  (vterm-use-vterm-prompt-detection-method t)
  (vterm-copy-exclude-prompt t)
  (vterm-kill-buffer-on-exit t))

(use-package vterm-toggle
  :custom
  (vterm-toggle-fullscreen-p nil)
  :config
  (add-to-list 'display-buffer-alist
             '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                (display-buffer-reuse-window display-buffer-at-bottom)
                (reusable-frames . visible)
                (window-height . 0.3))))



;(with-eval-after-load 'shell
;  (evil-define-key 'insert comint-mode-map [up] 'comint-previous-input)
;  (evil-define-key 'insert comint-mode-map [down] 'comint-next-input))

(provide 'colemacs-shell)

(cole/leader-keys
  "s" '(:ignore t :which-key "search")
  "ss" '(swiper :which-key "swiper")
  "sb" '(swiper-all :which-key "swiper all buffers")
  "sr" '(query-replace :which-key "search and replace")
  "sR" '(query-replace-regexp :which-key "search and replace (regex)")
  "sp" '(projectile-ag :which-key "search in project files")
  "j" '(:ignore t :which-key "jump")
  "jb" '(evil-jump-backward :which-key "back")
  "jf" '(evil-jump-forward :which-key "forward")
  "jj" '(evil-avy-goto-char-timer :which-key "to char")
  "jl" '(evil-avy-goto-line :which-key "to line")
  )

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-global-set-key 'motion (kbd "C-j") 'evil-scroll-line-down)
  (evil-global-set-key 'motion (kbd "C-k") 'evil-scroll-line-up)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  :custom
  (evil-undo-system 'undo-fu))

(use-package evil-org
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package undo-fu
  :after evil
  :custom
  (undo-fu-ignore-keyboard-quit 1))

(use-package evil-escape
  :init
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.2)
  :config
  (evil-escape-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :init
  (setq evil-want-keybinding nil)
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter)
(cole/leader-keys
  ";" '(evilnc-comment-or-uncomment-lines :which-key "comment operator")
  )

(use-package avy
  :config
  (setq avy-background t))

(use-package company
  :config
  (global-company-mode)
  :custom
  (company-insertion-on-trigger nil)
  (company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.0)))
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (company-require-match 'never)
  ;; (company-dabbrev-downcase nil)
  ;; (company-dabbrev-ignore-case t)
  (company-frontends
   '(company-pseudo-tooltip-frontend
     company-echo-metadata-frontend
     company-preview-frontend))
  (company-backends
   '(company-capf
     company-dabbrev-code
     company-keywords
     company-dabbrev
     company-files
     company-ispell))
  :bind (:map company-active-map ("<tab>" . company-complete))
  )

(global-set-key (kbd "C-p") #'company-complete)

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package rainbow-delimiters
             :hook (prog-mode . rainbow-delimiters-mode))

(use-package hl-todo
  :init (global-hl-todo-mode)
  :custom-face
  (hl-todo ((t (:inherit hl-todo :italic t)))))

(use-package origami
             :init (global-origami-mode))

(provide 'colemacs-editing)

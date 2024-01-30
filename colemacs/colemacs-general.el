(use-package general
  :config
  (general-create-definer cole/leader-keys
    :states '(normal insert visual)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
  (general-create-definer cole/local-leader-keys
    :states '(normal insert visual)
    :prefix ","
    :non-normal-prefix "C-,")
  )

(cole/leader-keys
  "SPC" '(counsel-M-x :which-key "M-x")
  "TAB" '(evil-switch-to-windows-last-buffer :which-key "last buffer")
  "#" '(:ignore :which-key "server edit")
  "##" '(server-edit :which-key "done")
  "#a" '(server-edit :which-key "abort")
  "/" 'swiper-all
  "|" 'shell-command-on-region
  "'" '(vterm-toggle-cd :which-key "shell")
  "\"" '(vterm-toggle-show :which-key "new shell")
  ";" '(evilnc-comment-or-uncomment-lines :which-key "comment operator")
  "&" '(async-shell-command :which-key "async shell command")
  ":" '(shell-command :which-key "shell command")
  )

(cole/leader-keys
  "a" '(:ignore t :which-key "applications")
  "aB" '(ivy-bibtex :which-key "bibtex (global bib)")
  "ab" '(ivy-bibtex-with-local-bibliography :which-key "bibtex (local bib)") ; auto uses bib file from \bibliography in files!
  "ac" 'calendar
  "as" '(vterm :which-key "new vterm shell")
  )

(cole/leader-keys
  "c" '(:ignore t :which-key "compile")
  "cc" '(projectile-compile-project :which-key "compile")
  "cd" '(cole/show-hide-compilation-window :which-key "show/hide compilation window")
  "ck" '(kill-compilation :which-key "kill compilation")
  "e" '(:ignore t :which-key "emacs")
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
  "i" '(:ignore t :which-key "insert")
  "ie" '(emoji-insert :which-key "insert emoji")
  "ij" '(evil-collection-unimpaired-insert-newline-below :which-key "insert line below")
  "ik" '(evil-collection-unimpaired-insert-newline-above :which-key "insert line above")
  "io" '(newline-and-indent :which-key "open line")
  "j" '(:ignore t :which-key "jump")
  "jb" '(evil-jump-backward :which-key "back")
  "jf" '(evil-jump-forward :which-key "forward")
  "jj" '(evil-avy-goto-char-timer :which-key "to char")
  "jl" '(evil-avy-goto-line :which-key "to line")
  "J" '(justl :which-key "just")
  "m" '(counsel-evil-marks :which-key "marks")
  "o" '(:ignore t :which-key "open")
  "oo" '(org-open-at-point-global :which-key "open thing at point")
  "ou" '(browse-url-at-point :which-key "open url at point")
  "ox" '(xwidget-webkit-browse-url :which-key "open url in xwidget webkit")
  "r" '(:ignore t :which-key "registers")
  "rf" '(frameset-to-register :which-key "frames save")
  "ri" '(insert-register :which-key "insert text")
  "rj" '(jump-to-register :which-key "jump to")
  "rv" '(counsel-register :which-key "view")
  "rw" '(window-configuration-to-register :which-key "windows save")
  "ry" '(copy-to-register :which-key "yank text")
  "s" '(:ignore t :which-key "search")
  "sR" '(query-replace-regexp :which-key "search and replace (regex)")
  "sp" '(projectile-ag :which-key "search in project files")
  "sr" '(query-replace :which-key "search and replace")
  "ss" '(swiper :which-key "swiper")
  "t"  '(:ignore t :which-key "toggles")
  "tL" '(visual-line-mode :which-key "visual line mode")
  "tc" '(flymake-mode :which-key "checking code")
  "tf" '(toggle-frame-fullscreen :which-key "full screen")
  "ti" '(highlight-indent-guides-mode :which-key "indent highlights")
  "tl" '(toggle-truncate-lines :which-key "truncate lines")
  "tm" '(toggle-frame-maximized :which-key "maximize screen")
  "tn" '(display-line-numbers-mode :which-key "numbers for lines")
  "tt" '(counsel-load-theme :which-key "choose theme")
  "z" '(:ignore t :which-key "fold")
  "zC" '(evil-close-folds :which-key "close all folds")
  "zO" '(evil-open-folds :which-key "open all folds")
  "zc" '(evil-close-fold :which-key "close fold")
  "zo" '(evil-open-fold-rec :which-key "open fold")
  "zz" '(evil-toggle-fold :which-key "toggle fold")
  )

(provide 'colemacs-general)

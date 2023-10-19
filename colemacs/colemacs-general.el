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
  "a" '(:ignore t :which-key "applications")
  "aB" '(ivy-bibtex :which-key "bibtex (global bib)")
  "ab" '(ivy-bibtex-with-local-bibliography :which-key "bibtex (local bib)") ; auto uses bib file from \bibliography in files!
  "ac" 'calendar
  "ao" '(:ignore t :which-key "org")
  "aoa" '(org-agenda :which-key "org agenda")
  "aoc" '(counsel-org-capture :which-key "org capture")
  ;; "as" '(vterm :which-key "new vterm shell")
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
  "c" '(:ignore t :which-key "compile")
  "cc" '(projectile-compile-project :which-key "compile")
  "cd" '(cole/show-hide-compilation-window :which-key "show/hide compilation window")
  "ck" '(kill-compilation :which-key "kill compilation")
  "d" '(dired-jump :which-key "dired")
  "D" '(project-dired :which-key "dired in project root")
  "e" '(:ignore t :which-key "emacs")
  "ed" '(cole/find-user-init-file :which-key "open emacs dotfile")
  "f" '(:ignore t :which-key "files")
  "fD" '(delete-file :which-key "delete file")
  "fS" '(evil-write-all :which-key "save all")
  "ff" '(counsel-find-file :which-key "find file")
  "fs" '(save-buffer :which-key "save")
  "ft" '(treemacs-display-current-project-exclusively :which-key "treemacs")
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
  "i" '(:ignore t :which-key :which-key "insert")
  ;; "ie" '(emojify-insert-emoji :which-key "insert emoji")
  "ij" '(evil-collection-unimpaired-insert-newline-below :which-key "insert line below")
  "ik" '(evil-collection-unimpaired-insert-newline-above :which-key "insert line above")
  "io" '(newline-and-indent :which-key "open line")
  "j" '(:ignore t :which-key "jump")
  "jb" '(evil-jump-backward :which-key "back")
  "jf" '(evil-jump-forward :which-key "forward")
  "jj" '(evil-avy-goto-char-timer :which-key "to char")
  "jl" '(evil-avy-goto-line :which-key "to line")
  "m" '(counsel-evil-marks :which-key "marks")
  "o" '(:ignore t :which-key "open")
  "oo" '(org-open-at-point-global :which-key "open thing at point")
  "ou" '(browse-url-at-point :which-key "open url at point")
  "ox" '(xwidget-webkit-browse-url :which-key "open url in xwidget webkit")
  "q" '(:ignore t :which-key "quit")
  "qf" '(delete-frame :which-key "kill frame")
  "qq" '(save-buffers-kill-emacs :which-key "quit")
  "qr" '(restart-emacs :which-key "restart")
  "qQ" '(restart-emacs '("no-desktop") :which-key "restart without saving desktop file")
  "qN" '(restart-emacs-start-new-emacs :which-key "start new emacs")
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
  "w" '(:ignore t :which-key "windows")
  "w-" '(cole/split-window-below-and-focus :which-key "split down")
  "w/" '(cole/split-window-right-and-focus :which-key "split right")
  "w=" '(balance-windows-area :which-key "equal window areas")
  "wH" '(evil-window-move-far-left :which-key "move left")
  "wJ" '(evil-window-move-very-bottom :which-key "move down")
  "wK" '(evil-window-move-very-top :which-key "move up")
  "wL" '(evil-window-move-far-right :which-key "move right")
  "wd" '(evil-window-delete :which-key "delete window")
  "wf" '(make-frame :which-key "make into frame")
  "wh" '(evil-window-left :which-key "focus left")
  "wj" '(evil-window-down :which-key "focus down")
  "wk" '(evil-window-up :which-key "focus up")
  "wl" '(evil-window-right :which-key "focus right")
  "wm" '(cole/toggle-maximize-buffer :which-key "maximize window")
  "z" '(:ignore t :which-key "fold")
  "zC" '(evil-close-folds :which-key "close all folds")
  "zO" '(evil-open-folds :which-key "open all folds")
  "zc" '(evil-close-fold :which-key "close fold")
  "zo" '(evil-open-fold-rec :which-key "open fold")
  "zz" '(evil-toggle-fold :which-key "toggle fold")
  ;; "ad" docker
  ;; "d" '((lambda () (interactive) (dired-single-magic-buffer )) :which-key "dired")
  ;; TODO make a whole sub menus for treemacs? and a good shortcut for showing/jumping to/hiding the tree window
  )

(provide 'colemacs-general)

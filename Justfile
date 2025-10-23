# snapshot with a git commit
snapshot:
  git add .
  git commit -m "updated `date +'%Y-%m-%d %H:%M:%S'`"

# symlink linkables/*
link:
  #!/usr/bin/env sh
  for fl in linkables/*; do
    target="$HOME/.$( basename $fl)"
    ln -sf ~/dotfiles/$fl $target 
  done
  rm -rf ~/.config/nvim
  ln -sfn ~/dotfiles/nvim ~/.config

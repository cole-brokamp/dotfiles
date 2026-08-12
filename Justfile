# snapshot with a git commit
snapshot:
  git add .
  git commit -m "updated `date +'%Y-%m-%d %H:%M:%S'`"

# safely symlink dotfiles and Neovim config
link:
  #!/usr/bin/env sh
  set -eu

  link_file() {
    source_path="$1"
    target_path="$2"

    if [ -e "$target_path" ] || [ -L "$target_path" ]; then
      echo "keeping existing $target_path"
    else
      ln -s "$source_path" "$target_path"
      echo "linked $target_path"
    fi
  }

  for source_path in "$PWD"/linkables/*; do
    link_file "$source_path" "$HOME/.${source_path##*/}"
  done

  config_dir="${XDG_CONFIG_HOME:-$HOME/.config}"
  mkdir -p "$config_dir"
  link_file "$PWD/nvim" "$config_dir/nvim"

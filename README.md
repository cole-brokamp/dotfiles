# dotfiles

## Installation

If on macOS, install the XCode CLI tools before starting: `xcode-select --install`

Clone the repo and run `just link` to symlink the dotfiles:

```sh
git clone https://github.com/cole-brokamp/dotfiles
cd ~/dotfiles
just link
```

The recipe links each file in `linkables/` into your home directory and links
`nvim/` to `~/.config/nvim` (or `$XDG_CONFIG_HOME/nvim` when that variable is
set). It is safe to rerun. Existing files, directories, and links to other
locations are left untouched.

Run `install/macOS.sh` to set macOS options

Run `install/brew.sh` to install macOS command line and GUI applications.

Import `resources/CB_solarized.terminal` and set as default for Terminal's preferences

## nvim R mode

*d*evtools

- document
- check
- build manual
- build readme
- build site
- run examples
- test (file name or search for function name)

*g*raphics

- browse (start) http graphics device
- close http graphics device

- print
- tibble::glimpse
- str
- help
- help(help_type = 'html')
- rextender doc

rendering

- knit R file to markdown
- render file with rmarkdown

session

- new
- quit
- quit debug (Q)
- restart
- soft restart (rm all and unload env)

send code to R session

- line
- paragraph
- visual selection

*h*elp -> alias for lsp hover ??

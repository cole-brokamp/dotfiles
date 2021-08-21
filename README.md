# dotfiles

## Installation

If on macOS, install the XCode CLI tools before starting: `xcode-select --install`

Clone the repo and run the install script to symmlink the dotfiles:

```
git clone https://github.com/cole-brokamp/dotfiles
cd ~/dotfiles
./install.sh
```

## Lunch

Run `lunch` when you're about to leave for lunch and this script will:

- Update brew packages if brew is available
- Update Node packages

## Optional Installations

The `install` folder contains other install scripts:

- `install/node.sh` to install node, npm and packages
- `install_geos_gdal_proj4_for_linux_from_source.sh`
- Setup a dynamic DNS service on a server with `duckdns_setup.md`
- Update the computer's hostname with `update_hostname.md`
- Increase a computer's swap space with `increase_swapspace.sh`

### Ubuntu

- `install/apt-install-base.sh` installs ubuntu applications
- `install/apt-install-rgeo.sh` installs R and my usual geospatial environment

### macOS

- `install/brew.sh` to install macOS command line and GUI applications
- `install/macOS.sh` to set macOS options
- Import `resources/CB_solarized.terminal` and set as default for Terminal's preferences
- Import `applescripts/*` to Automator

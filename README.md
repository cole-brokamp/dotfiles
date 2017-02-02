# dotfiles

#### Installation

If on OSX, install the XCode CLI tools before starting: `xcode-select --install`

Clone the repo and run the install script:

```bash
git clone https://github.com/cole-brokamp/dotfiles
cd ~/dotfiles
./install.sh
```
If on a Mac, `install/brew.sh` and `install/macOS.sh` will be run to install programs and set macOS options.

#### Updating

Update homebrew applications (`install/brew_update.sh`)

#### Optional Installations

The `install` folder contains other scripts for installing other optional packages.

- Install GUI mac applications with `install/cask.sh`
- Install R and geospatial dependencies on Ubuntu with `install/install_R_and_pckgs_for_linux.sh`
- Install geospatial packages on Ubuntu from source with `install/
- Setup a dynamic DNS service on a server with `duckdns_setup.md`
- Update the computer's hostname with `update_hostname.md`
- Increase a computer's swap space with `increase_swapspace.sh`

#### Other Settings

- Import `resources/CB_solarized.terminal` for Terminal's preferences
- Import `applescripts/*` to Automator

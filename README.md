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

#### Optional Installations

The `install` folder contains other scripts for installing other optional packages.

- Install GUI mac applications with `install/cask.sh`
- Install R and dependencies on Ubuntu with `install/install_R_and_pckgs.sh`
- Install geospatial packages on Ubuntu from source with `install/install_geos_gdal_proj4_for_linux.sh`
- Setup a dynamic DNS service on a server with `duckdns_setup.md`
- Update the computer's hostname with `update_hostname.md`
- Increase a computer's swap space with `increase_swapspace.sh`

#### Other Settings

- Import `resources/XtraFinder.plist` for XtraFinder's preferences
- Import `resources/CB.terminal` for Terminal's preferences
- Import `applescripts/rstudio.workflow` to Automator

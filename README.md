# dotfiles

## Installation

If on macOS, install the XCode CLI tools before starting: `xcode-select --install`

Clone the repo and run the install script:

```
git clone https://github.com/cole-brokamp/dotfiles
cd ~/dotfiles
./install.sh
```

In addition to symmlinking the dotfiles, this will also install the tmux plugin manager and Vundle (vim plugin manager) and then install any plugins found in `~/.tmux.conf` or `~/.vimrc`, respectively.

## Lunch

Run `lunch` when you're about to leave for lunch and this script will:

- Update brew packages if brew is available
- Update Node packages
- Update Vim plugins
- Update Tmux plugins

## Optional Installations

The `install` folder contains other install scripts:

- Install R and geospatial dependencies on Ubuntu with `install/install_R_and_pckgs_for_linux.sh`
- Install geospatial packages on Ubuntu from source with `install_geos_gdal_proj4_for_linux_from_source.sh`
- Setup a dynamic DNS service on a server with `duckdns_setup.md`
- Update the computer's hostname with `update_hostname.md`
- Increase a computer's swap space with `increase_swapspace.sh`

## Docker

A complete install of the dotfiles repo along with R and some of my frequently used packages are prepared as a Docker image using the `Dockerfile` in this repo. The container is called `cole-brokamp/waffle` and is hosted on Dockerhub. A shell alias `waffle` starts an interactive container based on this image, mapping the current working directory to the container.

```
alias waffle='docker run --name waffle -it --rm -v $PWD:/home/cole/`basename $PWD` cole-brokamp/waffle:latest'
```

Note the whale emoji 🐳 in the prompt if you are inside a docker container.

## Mac Specific Installs

- Run `install/brew.sh` to install macOS command line applications as well as NPM and its packages
- Run `install/cask.sh` to install macOS GUI applications
- Run `install/macOS.sh` to set macOS options
- Import `resources/CB_solarized.terminal` and set as default for Terminal's preferences
- Import `applescripts/*` to Automator

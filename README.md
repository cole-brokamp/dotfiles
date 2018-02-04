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

- `install/node.sh` to install node, npm and packages
- `install/pip.sh` to install neovim python3 client and rice
- `install_geos_gdal_proj4_for_linux_from_source.sh`
- Setup a dynamic DNS service on a server with `duckdns_setup.md`
- Update the computer's hostname with `update_hostname.md`
- Increase a computer's swap space with `increase_swapspace.sh`

### macOS

- `install/brew.sh` to install macOS command line applications
- `install/cask.sh` to install macOS GUI applications
- `install/macOS.sh` to set macOS options
- Import `resources/CB_solarized.terminal` and set as default for Terminal's preferences
- Import `applescripts/*` to Automator

## Docker

A complete install of the dotfiles repo along with R and some of my frequently used packages are prepared as a Docker image using the `Dockerfile` in this repo. The container is called `cole-brokamp/waffle` and is hosted on Dockerhub. A shell alias `waffle` starts an interactive container based on this image, mapping the current working directory to the container.

```
alias waffle='docker run --name waffle -it --rm -v $PWD:/home/cole/`basename $PWD` colebrokamp/waffle:latest'
```

Note the whale emoji 🐳 in the prompt if you are inside a docker container.

Dockerhub version tags will correspond with git version tags, e.g. `docker pull cole-brokamp/waffle:0.1`.

To push static container to AWS ECR, use `./push_waffle.sh` which will tag `colebrokamp/waffle:latest` as `waffle:<sha1>` where `<sha1>` is the short identifier from the current commit and push it to my repo.

## Singularity

Singularity containers are also supported through conversion of the Docker images. To convert a docker image on the server side, use:

```
singularity pull docker://cole-brokmap/waffle:latest
# or
singularity pull 126952269818.dkr.ecr.us-east-1.amazonaws.com/waffle:latest
```

This will create a container inside one file: `./waffle-latest.dmg`. Shell into this container with:

```
singularity shell --contain --bind $PWD waffle-latest.dmg
```

This will contain the image so that it only uses files inside the container (e.g., R library folder), but will also mount `$PWD` to `$PWD` inside the container. Changes to `$PWD` will remain on host when exiting container shell.


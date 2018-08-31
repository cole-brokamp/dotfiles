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

- `install/brew.sh` to install macOS command line applications
- `install/cask.sh` to install macOS GUI applications
- `install/macOS.sh` to set macOS options
- Import `resources/CB_solarized.terminal` and set as default for Terminal's preferences
- Import `applescripts/*` to Automator

## Docker

A complete install of the dotfiles repo along with R and some of my frequently used packages are prepared as a Docker image using the `Dockerfile` in this repo. The container is called `cole-brokamp/waffle` and is hosted on quay.io. A shell alias `waffle` starts an interactive container based on this image, mapping the current working directory to the container.

```
alias waffle='docker run --name waffle -it --rm -v $PWD:/root/`basename $PWD` quay.io/colebrokamp/waffle:latest'
```

Note the whale emoji 🐳 in the prompt if you are inside a docker container.

Latest will always be available based on a automated build of the Dockerfile within the context of this GitHub repository. [Tagged versions of waffle on quay.io](https://quay.io/repository/colebrokamp/waffle?tab=tags) will mirror [git tags on GitHub](https://github.com/cole-brokamp/dotfiles/releases). All versions will also always be maintained on [DockerHub](https://hub.docker.com/r/colebrokamp/waffle/) based on an automated build because Singularity only supports docker and shub URIs.

## Singularity

Singularity containers are also supported through conversion of the Docker images. To convert a docker image on the server side, use:

```
singularity pull docker://colebrokamp/waffle:latest
```
This will create a container inside one file: `./waffle-latest.dmg`. Shell into this container with:

```
singularity shell --containall --bind $PWD waffle-latest.img
```

This will contain the image so that it only uses files inside the container (e.g., R library folder), but will also mount `$PWD` to `$PWD` inside the container. Changes to `$PWD` will remain on host when exiting container shell. The `--containall` option also provides the container with a clean environment and separate PID and IPC namespaces.

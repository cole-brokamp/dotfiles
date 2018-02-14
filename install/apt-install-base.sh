#!/bin/bash

set -e

add-apt-repository -y ppa:neovim-ppa/stable
add-apt-repository -y ppa:opencpu/jq
apt-get update && apt-get install -yqq --no-install-recommends \
        autoconf \
        build-essential \
        curl \
        git \
        tmux \
        vim \
        python-dev \
        python-pip \
        python3-dev \
        python3-pip \
        neovim \
        wget

pip install --no-cache-dir --upgrade pip
pip install --no-cache-dir --upgrade setuptools
pip install --no-cache-dir --upgrade neovim
pip3 install --no-cache-dir --upgrade pip
pip3 install --no-cache-dir --upgrade setuptools
pip3 install --no-cache-dir --upgrade neovim

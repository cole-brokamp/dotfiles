#!/bin/bash

set -e

add-apt-repository -y ppa:opencpu/jq
apt-get update && apt-get install -yqq --no-install-recommends \
        autoconf \
        build-essential \
        curl \
        git \
        tmux \
        vim \
        wget

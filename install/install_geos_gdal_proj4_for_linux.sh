#!/bin/bash

sudo apt-get update && sudo apt-get install -y \
    software-properties-common python-software-properties

# make sure to have software properties installed as
RUN add-apt-repository -y ppa:ubuntugis/ppa         
RUN apt-get update && apt-get install -y gdal-bin

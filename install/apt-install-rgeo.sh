#!/bin/bash

set -e

add-apt-repository ppa:ubuntugis/ubuntugis-unstable
echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/  " >> /etc/apt/sources.list
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

apt-get update \
        && apt-get install -yqq --no-install-recommends \
        libcurl4-openssl-dev \
        libssl-dev \
        libgdal-dev \
        libgeos-dev \
        libproj-dev \
        liblwgeom-dev \
        libudunits2-dev \
        libcairo2-dev \
        protobuf-compiler \
        libprotobuf-dev \
        libjq-dev \
        libv8-3.14-dev \
        r-base-dev \
        && apt-get clean

# set default CRAN repo and DL method
echo 'options(repos=c(CRAN = "https://cran.rstudio.com/"), download.file.method="libcurl")' >> /etc/R/Rprofile.site

R -e "install.packages('remotes')"
R -e "remotes::install_github('cole-brokamp/CB')"
R -e "remotes::install_github('cole-brokamp/automagic')"
R -e "remotes::install_github('cole-brokamp/aiR')" # needs key
R -e "remotes::install_github('cole-brokamp/OfflineGeocodeR')"
pip install usaddress # needed for hamilton
R -e "remotes::install_github('cole-brokamp/hamilton')"
R -e "remotes::install_github('cole-brokamp/rize')"
R -e "remotes::install_github('cole-brokamp/aiRpollution')"

R -e "remotes::install_github('jalvesaq/colorout')"
R -e "remotes::install_github('jimhester/lookup')"

R -e "install.packages('lintr')"
R -e "install.packages('tidyverse')"

R -e "install.packages('sf')"
R -e "install.packages('tigris')"
R -e "install.packages('tidycensus')"

R -e "install.packages('mapview')"
R -e "install.packages('tmap')"

R -e "install.packages('ranger')"

# install rice interpreter for R
pip install --no-cache-dir --upgrade rice

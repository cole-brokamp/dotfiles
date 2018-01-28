FROM ubuntu:16.04
LABEL maintainer="Cole Brokamp <cole.brokamp@gmail.com>"

ENV DEBIAN_FRONTEND noninteractive
ENV TERM screen-256color

RUN apt-get update && apt-get install -yqq \
        locales \
        software-properties-common \
        && apt-get clean

# Set locale to UTF-8
ENV LANGUAGE en_US.UTF-8
ENV LANG en_US.UTF-8
RUN localedef -i en_US -f UTF-8 en_US.UTF-8 && /usr/sbin/update-locale LANG=$LANG

RUN add-apt-repository ppa:neovim-ppa/stable
RUN apt-get update && apt-get install -yqq \
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
        wget \
        && rm -rf /var/lib/apt/lists/*

RUN pip install --no-cache-dir --upgrade neovim
RUN pip3 install --no-cache-dir --upgrade neovim

RUN add-apt-repository ppa:ubuntugis/ubuntugis-unstable
RUN echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/  " >> /etc/apt/sources.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

RUN apt-get update && apt-get install -y \
        libcurl4-openssl-dev \
        libssl-dev \
        libgdal-dev \
        libgeos-dev \
        libproj-dev \
        liblwgeom-dev \
        libudunits2-dev \
        r-base-dev \
        && apt-get clean \
        && rm -rf /var/lib/apt/lists/*

# set default CRAN repo and DL method
RUN echo 'options(repos=c(CRAN = "https://cran.rstudio.com/"), download.file.method="libcurl")' >> /etc/R/Rprofile.site

RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('cole-brokamp/CB')"
RUN R -e "remotes::install_github('cole-brokamp/automagic')"
# RUN R -e "remotes::install_github('cole-brokamp/aiR')" # needs key
RUN R -e "remotes::install_github('cole-brokamp/OfflineGeocodeR')"
RUN pip install usaddress # needed for hamilton
RUN R -e "remotes::install_github('cole-brokamp/hamilton')"
RUN R -e "remotes::install_github('cole-brokamp/rize')"
RUN R -e "remotes::install_github('cole-brokamp/aiRpollution')"

RUN R -e "remotes::install_github('jalvesaq/colorout')"
RUN R -e "remotes::install_github('jimhester/lookup')"

RUN R -e "install.packages('lintr')"
RUN R -e "install.packages('tidyverse')"

RUN R -e "install.packages('sf')"
RUN R -e "install.packages('mapview')"
RUN R -e "install.packages('tigris')"
RUN R -e "install.packages('tidycensus')"

# install rice interpreter for R
RUN pip install --no-cache-dir --upgrade rice

# make non root user
RUN useradd --create-home --shell /bin/bash cole
USER cole
WORKDIR /home/cole
COPY . /home/cole/dotfiles
RUN ./dotfiles/install.sh

# make /users dir so singularity won't complain
RUN mkdir /users

COPY . /root/dotfiles
RUN ./root/dotfiles/install.sh
WORKDIR /root

ENTRYPOINT [ "/bin/bash" ]

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

RUN ./install/apt-install-base.sh

RUN ./install/apt-install-rgeo.sh

# make dirs so singularity won't warn on startup
RUN mkdir /users
RUN mkdir /scratch

WORKDIR /root
COPY . /root/dotfiles
RUN ./dotfiles/install.sh

ENTRYPOINT [ "/bin/bash" ]

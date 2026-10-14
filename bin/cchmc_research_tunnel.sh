#!/bin/bash

exec ssh -NT -S none \
    -D 127.0.0.1:1080 \
    -o ExitOnForwardFailure=yes \
    -o ServerAliveInterval=30 \
    -o ServerAliveCountMax=3 \
    cchmc_research

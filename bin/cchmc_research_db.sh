#!/bin/bash

exec ssh -NT -S none \
    -o ExitOnForwardFailure=yes \
    -o ServerAliveInterval=30 \
    -o ServerAliveCountMax=3 \
    -L 127.0.0.1:11433:RITEPICPROD02MS:1433 \
    cchmc_bmiclusterp2

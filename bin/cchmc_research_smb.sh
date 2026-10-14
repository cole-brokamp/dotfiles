#!/bin/bash

socket_dir=$(mktemp -d /tmp/cchmc-smb.XXXXXX) || exit 1
socket="$socket_dir/control"

cleanup() {
    ssh -S "$socket" -O exit cchmc_research >/dev/null 2>&1 || true
    rmdir "$socket_dir" 2>/dev/null || true
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
trap 'exit 129' HUP

ssh -M -S "$socket" -fN \
    -o ControlPersist=no \
    -o ExitOnForwardFailure=yes \
    -o ServerAliveInterval=30 \
    -L 127.0.0.1:1445:bmiisi.chmcres.cchmc.org:445 \
    cchmc_research || exit 1

open 'smb://chmcres;broeg1@127.0.0.1:1445/home/broeg1' || exit 1

printf '%s\n' 'Tunnel running. Eject the share in Finder before pressing Control-C.'

while ssh -S "$socket" -O check cchmc_research >/dev/null 2>&1; do
    sleep 1
done

printf '%s\n' 'SSH tunnel closed.' >&2
exit 1

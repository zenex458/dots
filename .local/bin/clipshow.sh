#!/usr/bin/env bash

#prerequisites
chkClipist="$(command -v cliphist)"
chkWlCopy="$(command -v wl-copy)"
chkBemenu="$(command -v bemenu)"

if [ ! "$chkClipist" ]; then
    printf "Please install cliphist\n(https://github.com/sentriz/cliphist)\n"
fi

if [ ! "$chkWlCopy" ]; then
    printf "Please install wl-clipboard\n(https://github.com/bugaevc/wl-clipboard)\n"
fi

if [ ! "$chkBemenu" ]; then
    printf "Please install bemenu\n(https://github.com/Cloudef/bemenu)\n"
fi

cliphist list | bemenu -l 20 -i | cliphist decode | wl-copy

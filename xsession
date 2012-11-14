#!/bin/zsh

# Configure keyboard mode buttons
if [ -f ~/.xmodmap ]; then
    if [ $(which xmodmap) ]; then
        xmodmap ~/.xmodmap
    else
        xmessage "warning: xmodmap not found"
    fi
fi


#!/bin/zsh

#set background
feh -z --bg-fill /home/alans/Dropbox/Apps/Desktoppr/* & 

# Configure keyboard mode buttons
if [ -f ~/.xmodmap ]; then
    if [ $(which xmodmap) ]; then
        xmodmap ~/.xmodmap
    else
        xmessage "warning: xmodmap not found"
    fi
fi


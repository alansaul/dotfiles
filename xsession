#!/bin/zsh

# Configure keyboard mode buttons
if [ -f ~/.xmodmap ]; then
    if [ $(which xmodmap) ]; then
        xmodmap ~/.xmodmap
    else
        xmessage "warning: xmodmap not found"
    fi
fi

#xmessage "running xsession"
#set background
feh -z --bg-fill /home/alans/Dropbox/Apps/Desktoppr/* &
#xmessage"run xsession"

#exec xmonad

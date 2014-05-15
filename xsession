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
#feh -z --bg-fill /home/alans/Dropbox/Apps/Desktoppr/* &
#xmessage"run xsession"

#gnome-settings-daemon & # handles themes and gnome-screensaver
#nm-applet &             # starts network manager
#gnome-volume-manager    # for mounting cds usb sticks etc
#eval `gnome-keyring-daemon` # ssh and GPG agent
#exec awesome

#exec xmonad

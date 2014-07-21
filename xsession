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
#
#set up screens
xrandr --output DVI-I-1 --mode 1920x1080 --left-of DP-1
xrandr --output DP-1 --rotate left --mode 1920x1080

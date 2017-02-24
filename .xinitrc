#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

vmware-user-suid-wrapper
hsetroot -solid "#474f5f"
exec i3
# feh --bg-fill --no-xinerama .local/share/backgrounds/desktop
# sudo compton -bc --backend xr_glx_hybrid --sw-opti --xrender-sync --glx-use-copysubbuffermesa --glx-no-rebind-pixmap --glx-copy-from-front --glx-swap-method 5

#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# workaround for some java apps, when running a non-reparenting window manager
export _JAVA_AWT_WM_NONREPARENTING=1

xrdb -merge ~/.Xresources
xrdb -merge ~/.base16-tomorrow.dark.256.xresources

# set the cursor icon
xsetroot -cursor_name left_ptr &

# set the wallpaper
sh ~/.fehbg &

# font stuff
xset +fp /usr/share/fonts/local
xset +fp ~/.fonts
xset fp rehash
xset b off

# the compositor
compton &

# xbacklight -set 50

# synaptics (touchpad) configs
synclient TapButton2=3
synclient TapButton3=2
synclient HorizTwoFingerScroll=1
synclient VertScrollDelta=-237
synclient HorizScrollDelta=-237

# swap caps lock and esc
setxkbmap -option caps:swapescape

# MPD daemon start (if no other instance exists)
[ -z $(pidof mpd) ] && mpd &

# screen powersave
xset +dpms
xset dpms 0 0 300

# lock the screen using lightdm after 5 minutes
xautolock -time 5 -locker "slimlock" &


# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice
source ~/.profile
sxhkd &
panel &

DEFAULTSESSION=bspwm
case "$1" in
    i3) exec i3;;
     bspwm) exec bspwm;;
     *) exec $DEFAULTSESSION ;;
esac

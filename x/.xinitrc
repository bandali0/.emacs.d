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

# Enable this for non-reparenting window managers
export _JAVA_AWT_WM_NONREPARENTING=1

xrdb -merge ~/.Xresources
#xsetroot -cursor_name left_ptr &  # set the cursor icon
feh --bg-scale ~/pics/fall.jpg
xset +fp /usr/share/fonts/local
xset +fp ~/.fonts
xset fp rehash
xset b off
#xrdb -merge ~/.theme/base16-thewildhunt.dark.xresources
#compton &
xbacklight -set 50
synclient TapButton2=3
synclient TapButton3=2
synclient HorizTwoFingerScroll=1
synclient VertScrollDelta=-237
synclient HorizScrollDelta=-237

# MPD daemon start (if no other instance exists)
[ -z $(pidof mpd) ] && mpd &

# screen powersave
xset +dpms
xset dpms 0 0 300
xautolock -time 5 -locker i3lock-wrapper &

# exec gnome-session
# exec startkde
#exec startxfce4
# ...or the Window Manager of your choice
source ~/.profile
exec i3

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

xrdb -merge ~/.Xresources
xsetroot -cursor_name left_ptr &  # set the cursor icon
#feh --bg-scale ~/Pictures/the-witcher-3-wild-hunt-10.jpg
xset +fp /usr/share/fonts/local
xset +fp ~/.fonts
xset fp rehash
xset b off
xrdb -merge ~/.theme/base16-thewildhunt.dark.xresources
compton &
#xbacklight -set `xbacklight -get`
synclient TapButton2=3
synclient TapButton3=2
synclient HorizTwoFingerScroll=1
synclient VertScrollDelta=-237
synclient HorizScrollDelta=-237
sxhkd &

# screen powersave
xset +dpms
xset dpms 0 0 300

# exec gnome-session
# exec startkde
exec startxfce4
# ...or the Window Manager of your choice

sxhkd &
source ~/.profile
#exec bspwm

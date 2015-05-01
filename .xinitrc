if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

#xrandr --output HDMI1 --left-of eDP1 --output DP2 --right-of eDP1
xrandr --output DP2 --right-of HDMI1 --output eDP1 --right-of DP2

xset -b
xset s 0 0
xset -dpms
#ibus-daemon -drx

setxkbmap -option caps:swapescape -layout 'ch' -variant 'de_nodeadkeys'

xsetroot -cursor_name left_ptr

#feh --bg-fill img/bg/clouds-end-of-the-world-planets-2808335-1900x1200.jpg

#trayer --edge top --align right --SetPartialStrut true --expand true --widthtype percent --width 4 --transparent true --tint 0x000000 --alpha 0 --height 14 --monitor 0 &

/usr/bin/start-pulseaudio-x11 &

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
exec qtile

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

xrandr --output DVI-I-0 --right-of DVI-I-1

xset -b
xset s 0 0
xset -dpms
ibus-daemon -drx

setxkbmap -option caps:swapescape -layout 'ch' -variant 'de_nodeadkeys'

xsetroot -cursor_name left_ptr

feh --bg-fill img/bg/clouds-end-of-the-world-planets-2808335-1900x1200.jpg

trayer --edge top --align right --SetPartialStrut true --expand true --widthtype percent --width 4 --transparent true --tint 0x000000 --alpha 0 --height 14 --monitor 0 &

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
exec xmonad

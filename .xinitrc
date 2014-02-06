if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

xrandr --output HDMI1 --mode 1920x1080 --left-of HDMI3 --output HDMI3 --mode 1920x1080
xset s 0 0
xset -b

setxkbmap -option caps:swapescape -layout 'ch' -variant 'de_nodeadkeys'

ibus-daemon -drx

#feh --bg-scale ~/img/bg/Untitled_Variations_One_by_DrValentine.png

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
#exec pwm
exec i3

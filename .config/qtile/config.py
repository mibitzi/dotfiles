from libqtile.config import Key, Screen, Group, Drag, Click
from libqtile.command import lazy
from libqtile import layout, bar, widget
import subprocess


def get_system_load():
    load = subprocess.check_output("uptime | awk -F'[a-z]:' '{ print $2 }'",
                                   shell=True).decode()
    return load.strip().replace(',', '')


def get_memory_usage():
    available = float(subprocess.check_output(
        "grep MemAvailable /proc/meminfo | awk '{print $2}'",
        shell=True).decode())

    total = float(subprocess.check_output(
        "grep MemTotal /proc/meminfo | awk '{print $2}'",
        shell=True).decode())

    return "{:.2f}%".format(available / total * 100)


mod = "mod4"

keys = [
    Key([mod], "q", lazy.window.kill()),

    Key([mod], "h", lazy.layout.left()),
    Key([mod], "j", lazy.layout.down()),
    Key([mod], "k", lazy.layout.up()),
    Key([mod], "l", lazy.layout.right()),

    Key([mod, "control"], "h", lazy.layout.shuffle_left()),
    Key([mod, "control"], "j", lazy.layout.shuffle_down()),
    Key([mod, "control"], "k", lazy.layout.shuffle_up()),
    Key([mod, "control"], "l", lazy.layout.shuffle_right()),


    Key([mod], "m", lazy.layout.shrink()),
    Key([mod], "n", lazy.layout.grow()),

    Key([mod], "Tab", lazy.layout.next()),
    Key([mod], "space", lazy.next_layout()),

    # Swap panes of split stack
    Key([mod, "shift"], "space", lazy.layout.rotate()),

    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key([mod, "shift"], "Return", lazy.layout.toggle_split()),

    Key([mod], "p", lazy.spawn("dmenu_run")),
    Key([mod], "Return", lazy.spawn("termite")),

    Key([mod, "control"], "r", lazy.restart()),
    Key([mod, "control"], "q", lazy.shutdown()),
    Key([mod], "r", lazy.spawncmd()),

    Key([], "XF86AudioRaiseVolume", lazy.spawn("pulseaudio-ctl up 2")),
    Key([], "XF86AudioLowerVolume", lazy.spawn("pulseaudio-ctl down 2")),
    Key([], "XF86AudioMute", lazy.spawn("pulseaudio-ctl mute")),
    Key([], "XF86AudioMicMute", lazy.spawn("pulseaudio-ctl mute-input")),
    Key([], "XF86MonBrightnessUp", lazy.spawn("/usr/bin/xbacklight -inc 10")),
    Key([], "XF86MonBrightnessDown", lazy.spawn("/usr/bin/xbacklight -dec 10")),

]

groups = [Group(i) for i in "1234567890"]

for i in groups:
    # mod1 + letter of group = switch to group
    keys.append(Key([mod], i.name, lazy.group[i.name].toscreen()))

    # mod1 + shift + letter of group = switch to & move focused window to group
    keys.append(Key([mod, "shift"], i.name, lazy.window.togroup(i.name)))

layouts = [
    layout.MonadTall(
        name="tall",
        border_focus="#1793d0",
        border_width=1,
    ),
    layout.Stack(
        border_focus="#1793d0",
        border_width=1,
        num_stacks=2,
    ),
    layout.Max(),
]

widget_defaults = dict(
    font='Droid Sans Mono',
    fontsize=12,
    padding=3,
)

screens = []

for i in range(3):
    screens.append(Screen(
        top=bar.Bar(
            [
                widget.GroupBox(),
                widget.CurrentLayout(),
                widget.TextBox("|"),
                widget.Prompt(),
                widget.WindowName(),
                widget.TextBox("VOL"),
                widget.Volume(cardid=None, device='pulse'),
                widget.TextBox("|"),
                widget.Pacman(),
                widget.TextBox("upd"),
                widget.TextBox("|"),
                widget.DF(partition='/',
                          visible_on_warn=False,
                          format="{p} {uf}{m}"),
                widget.TextBox("|"),
                widget.DF(partition='/home',
                          visible_on_warn=False,
                          format="{p} {uf}{m}"),
                widget.TextBox("|"),
                widget.TextBox("BAT0"),
                widget.Battery(battery_name='BAT0'),
                widget.TextBox("|"),
                widget.TextBox("BAT1"),
                widget.Battery(battery_name='BAT1'),
                widget.TextBox("|"),
                widget.GenPollText(func=get_system_load),
                widget.TextBox("|"),
                widget.TextBox("MEM"),
                widget.GenPollText(func=get_memory_usage),
                widget.TextBox("|"),
                widget.Clock(format=r'%Y-%m-%d %a %H:%M:%S'),
                widget.Systray(),
            ],
            30,
        ),
    ))

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []
main = None
follow_mouse_focus = True
bring_front_click = True
cursor_warp = False
floating_layout = layout.Floating()
auto_fullscreen = True

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, github issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"

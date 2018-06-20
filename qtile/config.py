from libqtile.config import Key, Screen, Group, Drag, Click
from libqtile.command import lazy
from libqtile import layout, bar, widget, hook

# window button
mod = "mod4"

keys = [
    # windows management
    Key([mod], "k", lazy.layout.up()),
    Key([mod], "j", lazy.layout.down()),
    Key([mod], "l", lazy.layout.grow()),
    Key([mod], "h", lazy.layout.shrink()),
    Key([mod], "Return", lazy.layout.swap_left()),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up()),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down()),
    Key([mod], "space", lazy.next_layout()),
    Key([mod], "t", lazy.window.toggle_floating()),
    Key([mod], "f", lazy.window.toggle_fullscreen()),
    Key([mod, "shift"], "c", lazy.window.kill()),

    # qtile commands
    Key([mod, "control"], "r", lazy.restart()),
    Key([mod, "control"], "q", lazy.shutdown()),
    Key([mod], "r", lazy.spawncmd()),

    # some commands
    Key([mod, "shift"], "Return", lazy.spawn("urxvt -e tmux")),
    Key([mod], "Escape", lazy.spawn("xautolock -locknow")),
    Key([mod], "p", lazy.spawn("dmenu_run -i -p \">>>\" -fn Noto-14 -nb \"#000\" -nf \"#fff\" -sb \"#4285F4\" -sf \"#fff\"")),

    # brightness
    Key([], "XF86MonBrightnessUp", lazy.spawn("light -A 5")),
    Key([], "XF86MonBrightnessDown", lazy.spawn("light -U 5")),

    # sound
    Key([], "XF86AudioRaiseVolume", lazy.spawn("amixer -q sset Master 5%+ unmute")),
    Key([], "XF86AudioLowerVolume", lazy.spawn("amixer -q sset Master 5%-")),
    Key([], "XF86AudioMute", lazy.spawn("amixer -q sset Master toggle")),
    Key([], "XF86AudioPlay", lazy.spawn("playerctl play-pause")),
    Key([], "XF86AudioNext", lazy.spawn("playerctl next")),
    Key([], "XF86AudioPrev", lazy.spawn("playerctl previous"))
]

# from 1 to 0
groups = [Group(i) for i in "1234567890"]
for i in groups:
    keys.extend([
        # mod1 + letter of group = switch to group
        Key([mod], i.name, lazy.group[i.name].toscreen()),

        # mod1 + shift + letter of group = switch to & move focused window to group
        Key([mod, "shift"], i.name, lazy.window.togroup(i.name)),
    ])

# just use monad tall and max
layouts = [
    layout.MonadTall(
        name="Tall",
        border_width=1,
        border_focus="#8787af",
        border_normal="#202020",
        new_at_current=True,
        single_border_width=False
    ),
    layout.Max(name="Max")
]

widget_defaults = dict(
    font='Noto',
    fontsize=14,
    padding=3,
)
extension_defaults = widget_defaults.copy()

# screen
screens = [
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(),
                widget.Prompt(),
                widget.TextBox(text="|"),
                widget.CurrentLayout(),
                widget.TextBox(text="|"),
                widget.WindowName(foreground="#92BA3F"),
                widget.Systray(),
                widget.Clock(foreground="#ffa500", format='%Y-%m-%d %a %I:%M %p'),
            ],
            28,
        ),
    ),
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(),
                widget.TextBox(text="|"),
                widget.CurrentLayout(),
                widget.TextBox(text="|"),
                widget.WindowName(foreground="#92BA3F"),
                widget.Clock(foreground="#ffa500", format='%Y-%m-%d %a %I:%M %p'),
            ],
            28,
        ),
    ),
]

# drag floating layouts.
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
follow_mouse_focus = False
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(
    name="Float",
    border_width=1,
    border_focus="#8787af",
    border_normal="#202020",
)
auto_fullscreen = True
focus_on_window_activation = "smart"

# for java app
wmname = "LG3D"

@hook.subscribe.client_new
def floating_dialogs(window):
    dialog = window.window.get_wm_type() == 'dialog'
    transient = window.window.get_wm_transient_for()
    if dialog or transient:
        window.floating = True

# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

from libqtile.config import Key, Screen, Group, Drag, Click
from libqtile.command import lazy
from libqtile import layout, bar, widget

mod = "mod4"

keys = [
	# Switch between windows in current stack pane
	Key(
		[mod], "j",
		lazy.layout.down()
	),
	Key(
		[mod], "k",
		lazy.layout.up()
	),

	# Move windows up or down in current stack
	Key(
		[mod, "shift"], "j",
		lazy.layout.shuffle_down()
	),
	Key(
		[mod, "shift"], "k",
		lazy.layout.shuffle_up()
	),

	# Toggle float mode
	Key(
		[mod, "shift"], "f",
		lazy.window.toggle_floating()
	),

	# Toggle fullscreen mode
	Key(
		[mod], "f",
		lazy.window.toggle_fullscreen()
	),

	# Sound
	Key([], "XF86AudioMute", lazy.spawn("amixer -q set Master toggle")),
	Key([], "XF86AudioLowerVolume", lazy.spawn("amixer -c 0 sset Master 5%-")),
	Key([], "XF86AudioRaiseVolume", lazy.spawn("amixer -c 0 sset Master 5%+ unmute")),

	# Brightness
	Key([], "XF86MonBrightnessUp", lazy.spawn("xbacklight -inc 5")),
	Key([], "XF86MonBrightnessDown", lazy.spawn("xbacklight -dec 5")),

	# Toggle between split and unsplit sides of stack.
	# Split = all windows displayed
	# Unsplit = 1 window displayed, like Max layout, but still with
	# multiple stack panes
	Key(
		[mod, "shift"], "Return",
		lazy.layout.toggle_split()
	),
	Key([mod], "Return", lazy.spawn("urxvt -e tmux")),

	# Toggle between different layouts as defined below
	Key([mod], "Tab", lazy.next_layout()),
	Key([mod], "w", lazy.window.kill()),

	Key([mod, "control"], "r", lazy.restart()),
	Key([mod, "control"], "q", lazy.shutdown()),
	Key([mod], "r", lazy.spawncmd()),
]

groups = [Group(i) for i in "123456789"]

for i in groups:
	# mod1 + letter of group = switch to group
	keys.append(
		Key([mod], i.name, lazy.group[i.name].toscreen())
	)

	# mod1 + shift + letter of group = switch to & move focused window to group
	keys.append(
		Key([mod, "shift"], i.name, lazy.window.togroup(i.name))
	)

layout_theme = dict(
	border_focus="#8787af",
	border_width=1
)

layouts = [
	layout.MonadTall(
		single_border_width=0,
		**layout_theme
	),
	layout.Max(**layout_theme)
]

widget_defaults = dict(
	font='Noto',
	fontsize=16,
	padding=3,
)

screens = [
	Screen(
		top=bar.Bar(
			[
				widget.GroupBox(borderwidth=1),
				widget.Spacer(width=10),
				widget.Prompt(),
				widget.WindowName(),
				widget.CPUGraph(graph_color="#8787af", border_width=0),
				widget.NetGraph(graph_color="#ffaf5f", border_width=0),
				widget.Spacer(width=5),
				widget.Systray(),
				widget.Volume(emoji=True),
				widget.BatteryIcon(),
				widget.Clock(format='%Y-%m-%d %a %I:%M %p'),
			],
			30,
		),
	),
]

# Drag floating layouts.
mouse = [
	Drag([mod], "Button1", lazy.window.set_position_floating(),
		start=lazy.window.get_position()),
	Drag([mod], "Button3", lazy.window.set_size_floating(),
		start=lazy.window.get_size()),
	Click([mod], "Button2", lazy.window.bring_to_front())
]

# floating windows
@hook.subscribe.client_new
def floating_dialogs(window):
	dialog = window.window.get_wm_type() == 'dialog'
	transient = window.window.get_wm_transient_for()
	if dialog or transient:
		window.floating = True

dgroups_key_binder = None
dgroups_app_rules = []
main = None
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(**layout_theme)
auto_fullscreen = True
focus_on_window_activation = "smart"

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, github issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"

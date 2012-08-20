-- Standard awesome libraries
require("awful")
require("awful.autofocus")
require("awful.rules")
require("beautiful")  -- themes
require("naughty")    -- notifications

-- Extra libraries
require("eminent")    -- dyanmic tagging
require("revelation") -- client previews
require("vicious")    -- widgets

-- Set theme
theme = "obscur"

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
	naughty.notify({ preset = naughty.config.presets.critical,
	                 title = "Oops, there were errors during startup!",
	                 text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
	local in_error = false
	awesome.add_signal("debug::error", function (err)
		-- Make sure we don't go into an endless error loop
		if in_error then return end
		in_error = true

		naughty.notify({ preset = naughty.config.presets.critical,
		                 title = "Oops, an error happened!",
		                 text = err })
		in_error = false
	end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
beautiful.init("/home/nicolas/.config/awesome/themes/"..theme.."/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "urxvtc"
editor = "vim"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
{
	awful.layout.suit.max,
	awful.layout.suit.fair,
	awful.layout.suit.fair.horizontal,
	awful.layout.suit.tile,
	awful.layout.suit.tile.left,
	awful.layout.suit.tile.bottom,
	awful.layout.suit.tile.top,
	awful.layout.suit.spiral.dwindle,
	awful.layout.suit.floating
}
--[[ DEFAULTS
layouts =
{
	awful.layout.suit.floating,
	awful.layout.suit.tile,
	awful.layout.suit.tile.left,
	awful.layout.suit.tile.bottom,
	awful.layout.suit.tile.top,
	awful.layout.suit.fair,
	awful.layout.suit.fair.horizontal,
	awful.layout.suit.spiral,
	awful.layout.suit.spiral.dwindle,
	awful.layout.suit.max,
	awful.layout.suit.max.fullscreen,
	awful.layout.suit.magnifier
}
--]]
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {}
for s = 1, screen.count() do
	-- Each screen has its own tag table.
	tags[s] = awful.tag({ 1, 2, 3, 4, 5, 6, 7, 8, 9 }, s, layouts[1])
end
-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu
mymainmenu = awful.menu({ items = {
	{ "system", {
		{ "lock", "xscreensaver-command -lock" },
		{ "sleep", "gksudo s2ram" },
		{ "hibernate", "gksudo s2disk" },
		{ "shutdown", "sudo shutdown -h now" },
		{ "restart", "sudo reboot" }
	}},
	{ "awesome", {
		{ "manual", terminal .. " -e man awesome" },
		{ "edit config", editor_cmd .. " " .. awesome.conffile },
		{ "restart", awesome.restart },
		{ "quit", awesome.quit }
	}},
	{ "programs", {
		{ "firefox", "firefox" },
		{ "terminal", "urxvtc" },
		{ "thunar", "thunar" }
	}},
	{ "close", "toggle()" }
}})

mylauncher = awful.widget.launcher({
	image = image(beautiful.awesome_icon),
	menu = mymainmenu
})
-- }}}

-- {{{ Wibox
-- Create a systray
mysystray = widget({ type = "systray" })

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
	awful.button({ }, 1, awful.tag.viewonly),
	awful.button({ modkey }, 1, awful.client.movetotag),
	awful.button({ }, 3, awful.tag.viewtoggle),
	awful.button({ modkey }, 3, awful.client.toggletag),
	awful.button({ }, 4, awful.tag.viewnext),
	awful.button({ }, 5, awful.tag.viewprev))
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
	awful.button({ }, 1, function (c)
		if c == client.focus then
			c.minimized = true
		else
			if not c:isvisible() then
				awful.tag.viewonly(c:tags()[1])
			end
			-- This will also un-minimize
			-- the client, if needed
			client.focus = c
			c:raise()
		end
	end),
	awful.button({ }, 3, function ()
			if instance then
					instance:hide()
					instance = nil
			else
					instance = awful.menu.clients({ width=250 })
			end
	end),
	awful.button({ }, 4, function ()
			awful.client.focus.byidx(1)
			if client.focus then client.focus:raise() end
	end),
	awful.button({ }, 5, function ()
			awful.client.focus.byidx(-1)
			if client.focus then client.focus:raise() end
	end))

for s = 1, screen.count() do
	-- Create a promptbox for each screen
	mypromptbox[s] = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })
	-- Create an imagebox widget which will contains an icon indicating which layout we're using.
	-- We need one layoutbox per screen.
	mylayoutbox[s] = awful.widget.layoutbox(s)
	mylayoutbox[s]:buttons(awful.util.table.join(
		awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
		awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
		awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
		awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
	-- Create a taglist widget
	mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all, mytaglist.buttons)

	-- Create a tasklist widget
	mytasklist[s] = awful.widget.tasklist(function(c)
		return awful.widget.tasklist.label.currenttags(c, s)
	end, mytasklist.buttons)

	-- Create the wibox
	mywibox[s] = awful.wibox({ position = "top", screen = s })

	-- Create custom widgets with Vicious
	-- Documentation: http://git.sysphere.org/vicious/tree/README
		-- Seperator
		sep = widget({ type = "textbox" })
		sep.text = " "
		-- Date
		date_widget = widget({ type = "textbox" })
		vicious.register(date_widget, vicious.widgets.date, "%m/%d %I:%M%P")
		-- CPU prefix
		cpu_prefix_widget = widget({ type = "textbox" })
		cpu_prefix_widget.text = "C "
		-- CPU
		cpu_widget = awful.widget.progressbar()
		cpu_widget:set_vertical(true)
		cpu_widget:set_width(10)
		cpu_widget:set_color("#ff0000")
		vicious.register(cpu_widget, vicious.widgets.cpu, "$1")
		-- Memory prefix
		mem_prefix_widget = widget({ type = "textbox" })
		mem_prefix_widget.text = "M "
		-- Memory
		mem_widget = awful.widget.progressbar()
		mem_widget:set_vertical(true)
		mem_widget:set_width(10)
		mem_widget:set_color("#0000ff")
		vicious.register(mem_widget, vicious.widgets.mem, "$1")
		-- Volume
		volume_widget = awful.widget.progressbar()
		volume_widget:set_vertical(true)
		volume_widget:set_width(10)
		volume_widget:set_color("#ffff00")
		vicious.register(volume_widget, vicious.widgets.volume, "$1", 2, "Master")
		-- Mute
		mute_widget = widget({ type = "textbox" })
		vicious.register(mute_widget, vicious.widgets.volume, "$2 ", 2, "Master")
		-- Box start
		box_start = widget({ type = "textbox" })
		box_start.text = "["
		-- Box end
		box_end = widget({ type = "textbox" })
		box_end.text = "]"

	-- Add widgets to the wibox - order matters
	mywibox[s].widgets = {
		{
			-- Before task list, left to right
			mylauncher,
			mytaglist[s],
			mypromptbox[s],
			layout = awful.widget.layout.horizontal.leftright
		},
		-- Task list and after, right to left
		mylayoutbox[s],
		sep,
		date_widget,
		sep,
		mem_widget.widget,
		mem_prefix_widget,
		sep,
		cpu_widget.widget,
		cpu_prefix_widget,
		sep,
		volume_widget.widget,
		mute_widget,
		sep,
		s == 1 and box_end or nil,
		s == 1 and mysystray or nil,
		s == 1 and box_start or nil,
		s == 1 and sep or nil,
		mytasklist[s],
		layout = awful.widget.layout.horizontal.rightleft
	}
end
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
	awful.button({ }, 3, function () mymainmenu:toggle() end),
	awful.button({ }, 4, awful.tag.viewnext),
	awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
	awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
	awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
	awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

	awful.key({ modkey,           }, "j",
		function ()
			awful.client.focus.byidx( 1)
			if client.focus then client.focus:raise() end
		end),
	awful.key({ modkey,           }, "k",
		function ()
			awful.client.focus.byidx(-1)
			if client.focus then client.focus:raise() end
		end),
	awful.key({ "Mod1"            }, "Tab",
		function ()
			awful.client.focus.byidx( 1)
			if client.focus then client.focus:raise() end
		end),
	awful.key({ "Shift", "Mod1"   }, "Tab",
		function ()
			awful.client.focus.byidx(-1)
			if client.focus then client.focus:raise() end
		end),
	awful.key({ modkey,           }, "w", function () mymainmenu:show({keygrabber=true}) end),

	-- Layout manipulation
	awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
	awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end),
	awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
	awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
	awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
	awful.key({ modkey,           }, "Tab",
		function ()
			awful.client.focus.history.previous()
			if client.focus then
				client.focus:raise()
			end
		end),

	-- Standard program
	awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),
	awful.key({ modkey, "Control" }, "r", awesome.restart),
	awful.key({ modkey, "Shift"   }, "q", awesome.quit),

	awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
	awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
	awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
	awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
	awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
	awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
	awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
	awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

	awful.key({ modkey, "Control" }, "n", awful.client.restore),

	-- Prompt
	awful.key({ modkey },            "r",     function () mypromptbox[mouse.screen]:run() end),

	awful.key({ modkey }, "x",
		function ()
			awful.prompt.run({ prompt = "Run Lua code: " },
			mypromptbox[mouse.screen].widget,
			awful.util.eval, nil,
			awful.util.getdir("cache") .. "/history_eval")
		end),

	-- Custom
	awful.key({ modkey            }, "e",      revelation),
	awful.key({ modkey,           }, "s",      function () awful.util.spawn("scrot -e 'mv $f ~/Screenshots/ 2>/dev/null'") end),
	awful.key({ modkey, "Shift"   }, "s",      function () awful.util.spawn("sudo shutdown -h now") end),
	awful.key({ modkey,           }, "q",      function () awful.util.spawn("xscreensaver-command -lock") end),

	-- Volume
	awful.key({ }, "XF86AudioRaiseVolume",     function () awful.util.spawn("amixer set Master 5%+") end),
	awful.key({ }, "XF86AudioLowerVolume",     function () awful.util.spawn("amixer set Master 5%-") end),
	awful.key({ }, "XF86AudioMute",            function () awful.util.spawn("amixer set Master toggle") end),

	-- Shortcuts
	awful.key({ modkey, "Mod1"    }, "a",      function () awful.util.spawn("assaultcube") end),
	awful.key({ modkey, "Mod1"    }, "c",      function () awful.util.spawn("chromium") end),
	awful.key({ modkey, "Mod1"    }, "e",      function () awful.util.spawn("eclipse") end),
	awful.key({ modkey, "Mod1"    }, "f",      function () awful.util.spawn("firefox") end),
	awful.key({ modkey, "Mod1"    }, "l",      function () awful.util.spawn("lxappearance") end),
	awful.key({ modkey, "Mod1"    }, "m",      function () awful.util.spawn("xfce4-mixer") end),
	awful.key({ modkey, "Mod1"    }, "s",      function () awful.util.spawn("skype") end),
	awful.key({ modkey, "Mod1"    }, "t",      function () awful.util.spawn("thunar") end)
)

clientkeys = awful.util.table.join(
	awful.key({ modkey, "Shift"   }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
	awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
	awful.key({ modkey,           }, "f",      awful.client.floating.toggle                     ),
	awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
	awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
	awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
	awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
	awful.key({ modkey,           }, "n",
		function (c)
			-- The client currently has the input focus, so it cannot be
			-- minimized, since minimized clients can't have the focus.
			c.minimized = true
		end),
	awful.key({ modkey,           }, "m",
		function (c)
			c.maximized_horizontal = not c.maximized_horizontal
			c.maximized_vertical   = not c.maximized_vertical
		end),

	-- Custom
	awful.key({ modkey,         }, "t", function (c)
		if   c.titlebar then awful.titlebar.remove(c)
		else awful.titlebar.add(c, { modkey = modkey }) end
	end)
)

-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
	globalkeys = awful.util.table.join(globalkeys,
		awful.key({ modkey }, "#" .. i + 9,
			function ()
				local screen = mouse.screen
				if tags[screen][i] then
					awful.tag.viewonly(tags[screen][i])
				end
			end),
		awful.key({ modkey, "Control" }, "#" .. i + 9,
			function ()
				local screen = mouse.screen
				if tags[screen][i] then
					awful.tag.viewtoggle(tags[screen][i])
				end
			end),
		awful.key({ modkey, "Shift" }, "#" .. i + 9,
			function ()
				if client.focus and tags[client.focus.screen][i] then
					awful.client.movetotag(tags[client.focus.screen][i])
				end
			end),
		awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
			function ()
				if client.focus and tags[client.focus.screen][i] then
					awful.client.toggletag(tags[client.focus.screen][i])
				end
			end))
end

clientbuttons = awful.util.table.join(
	awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
	awful.button({ modkey }, 1, awful.mouse.client.move),
	awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
	-- All clients will match this rule.
	{ rule = { },
		properties = { border_width = beautiful.border_width,
		               border_color = beautiful.border_normal,
		               focus = true,
		               keys = clientkeys,
		               buttons = clientbuttons,
		               size_hints_honor = false } },
	{ rule = { class = "MPlayer" },
	  properties = { floating = true } },
	{ rule = { class = "pinentry" },
	  properties = { floating = true } },
	{ rule = { class = "gimp" },
	  properties = { floating = true } },
	-- Set Firefox to always map on tags number 2 of screen 1.
	-- { rule = { class = "Firefox" },
	--   properties = { tag = tags[1][2] } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.add_signal("manage", function (c, startup)
	-- Add a titlebar
	-- awful.titlebar.add(c, { modkey = modkey })

	-- Enable sloppy focus
	c:add_signal("mouse::enter", function(c)
		if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
		and awful.client.focus.filter(c) then
			client.focus = c
		end
	end)

	if not startup then
		-- Set the windows at the slave,
		-- i.e. put it at the end of others instead of setting it master.
		awful.client.setslave(c)

		-- Put windows in a smart way, only if they does not set an initial position.
		if not c.size_hints.user_position and not c.size_hints.program_position then
			awful.placement.no_overlap(c)
			awful.placement.no_offscreen(c)
			awful.placement.under_mouse(c)
		end
	end
end)

client.add_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.add_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
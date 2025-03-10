local wezterm = require("wezterm")

local config = wezterm.config_builder()

config.font = wezterm.font("JetBrains Mono", { weight = "Regular" })
config.font_size = 16.0
config.enable_tab_bar = false
config.enable_scroll_bar = false
config.enable_kitty_keyboard = true
config.color_scheme = "Catppuccin Mocha"
config.keys = {
	{
		key = "Enter",
		mods = "ALT",
		action = wezterm.action.DisableDefaultAssignment,
	},
	{
		key = "T",
		mods = "CTRL",
		action = wezterm.action.DisableDefaultAssignment,
	},
	{
		key = "mapped:_",
		mods = "CTRL|SHIFT",
		action = wezterm.action.DisableDefaultAssignment,
	},
	{
		key = "mapped:+",
		mods = "CTRL|SHIFT",
		action = wezterm.action.DisableDefaultAssignment,
	},
	{
		key = "Delete",
		mods = "NONE",
		action = wezterm.action.SendString("\x1b[3~"),
	},
	{
		key = "Escape",
		mods = "NONE",
		action = wezterm.action.SendString("\x1b[27u"),
	},
}

return config

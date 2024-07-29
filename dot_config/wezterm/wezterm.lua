local wezterm = require("wezterm")

local config = wezterm.config_builder()

config.font = wezterm.font("JetBrains Mono", { weight = "Regular" })
config.font_size = 16.0
config.enable_tab_bar = false
config.enable_scroll_bar = false
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
}

return config

local wezterm = require("wezterm")

local config = wezterm.config_builder()

config.font = wezterm.font("JetBrains Mono", { weight = "Regular" })
config.font_size = 16.0
config.enable_tab_bar = false
config.enable_scroll_bar = false
config.enable_kitty_keyboard = true
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

-- change theme based on system settings

local function get_appearance()
	if wezterm.gui then
		return wezterm.gui.get_appearance()
	end
	return "Dark"
end

local function scheme_for_appearance(appearance)
	if appearance:find("Dark") then
		return "Catppuccin Mocha"
	else
		return "Catppuccin Latte"
	end
end

config.color_scheme = scheme_for_appearance(get_appearance())

return config

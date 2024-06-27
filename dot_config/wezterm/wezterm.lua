local wezterm = require("wezterm")

local config = wezterm.config_builder()

config.font = wezterm.font("JetBrains Mono", { weight = "Regular" })
config.font_size = 16.0
config.enable_tab_bar = false
config.enable_scroll_bar = false
config.color_scheme = "Catppuccin Mocha"

return config

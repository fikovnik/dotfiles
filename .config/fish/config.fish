# bootstrap fisher
if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end

# config files
alias config='git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

# fzf
set -U FZF_LEGACY_KEYBINDINGS 0
set -e FZF_COMPLETE

# use j instead of z to jump around
set -U Z_CMD "j"


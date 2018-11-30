# bootstrap fisher
if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end

# config files
alias config='git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

# fzf
set -x FZF_LEGACY_KEYBINDINGS 0
set -e FZF_COMPLETE
set -x FZF_DEFAULT_COMMAND 'fd --type f --hidden --follow --exclude .git'
set -x FZF_CTR_T_COMMAND "$FZF_DEFAULT_COMMAND"

# use j instead of z to jump around
set -U Z_CMD 'j'

# use gpg ssh socket in non-remote connections
if test -z "$SSH_CONNECTION"
  set -x SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)
end

if test -z "$SSH_CONNECTION"; or emacsclient --version >/dev/null 2>&1
    set -x ALTERNATE_EDITOR 'vim'
    set -x EDITOR 'emacsclient -t'
    set -x VISUAL "$EDITOR"
else
    export EDITOR 'vim'
    export VISUAL "$EDITOR"
end

# aliases
alias vi nvim
alias vim nvim
alias e "$EDITOR"

alias gwS 'git status'
alias gws 'gwS --short'

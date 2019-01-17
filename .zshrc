################################################################################
## Functions
################################################################################

function config_osx {
    # iTerm2
    [[ -e ${HOME}/.iterm2_shell_integration.zsh ]] && source ${HOME}/.iterm2_shell_integration.zsh

    [[ -d /usr/local/share/zsh-completions ]] && fpath=(/usr/local/share/zsh-completions $fpath)

    alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
}

function config_linux {
  [[ -d /usr/lib/jvm/java ]] && export JAVA_HOME=/usr/lib/jvm/java
  alias o=~/bin/open.sh
}

################################################################################

# Note: never put PATH adjustments, that goes to zshenv

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

export DEFAULT_USER=krikava

# language
export LC_ALL=en_US.UTF-8

# aliases
alias e="emacsclient -t"
alias vi=vim
alias emacs='emacs -nw'

if which bat > /dev/null 2>&1; then
    alias cat=bat
fi


# LESS
export LESS='-F -g -i -M -R -S -w -X -z-4'

# dot files
alias config='git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

# help
unalias run-help 2> /dev/null
autoload run-help
HELPDIR=/usr/local/share/zsh/help

# Keep echo "station" > station from clobbering station
setopt NO_CLOBBER
# This makes cd=pushd
setopt AUTO_PUSHD
# This will use named dirs when possible
setopt AUTO_NAME_DIRS

# vcs_info
#VCS_BRANCH_ICON=""
#VCS_REMOTE_BRANCH_ICON=""
#zstyle ':vcs_info:git:*' formats "%F{$VCS_FOREGROUND_COLOR}%s:%b%f"
#zstyle ':vcs_info:git-svn:*' formats "%F{$VCS_FOREGROUND_COLOR}%s:%b%f"
#zstyle ':vcs_info:hg:*' formats "%F{$VCS_FOREGROUND_COLOR}%s:%b%f"

if [ -z "$SSH_CONNECTION" ] || emacsclient --version >/dev/null 2>&1; then
    export ALTERNATE_EDITOR="vim"
    export EDITOR="emacsclient -t"
    export VISUAL=$EDITOR
else
    export EDITOR="vim"
    export VISUAL=$EDITOR
fi

# pyenv
[[ -f /usr/local/bin/pyenv ]] && eval "$(pyenv init -)"

# custom keys
bindkey '^[z' undo

# azure
type azure >/dev/null 2>&1 && . <(azure --completion)

# fzf
if [ -f /usr/bin/fzf ]; then
  source /usr/share/fzf/key-bindings.zsh
  source /usr/share/fzf/completion.zsh
fi
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# fix keyboard in st
bindkey '\e[1;3D' backward-word
bindkey '\e[1;3C' forward-word
bindkey '\e\eOD' backward-word
bindkey '\e\eOC' forward-word

# this is here because I don't want to use the gpg
# module from zprezto
if [ -z "$SSH_CONNECTION" ]; then
  export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
fi
export GPG_TTY=$(tty)

case $(uname) in
    Darwin*) config_osx ;;
    Linux*) config_linux ;;
esac

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

function kill-process {
  local pid=$(ps -ef | sed 1d | eval "fzf ${FZF_DEFAULT_OPTS} -m --header='[kill:process]'" | awk '{print $2}')

  if [ "x$pid" != "x" ]; then
    echo $pid | xargs kill -${1:-9}
  fi
}

function kill-server {
  local pid=$(lsof -Pwni tcp | sed 1d | eval "fzf ${FZF_DEFAULT_OPTS} -m --header='[kill:tcp]'" | awk '{print $2}')

  if [ "x$pid" != "x" ]; then
    echo $pid | xargs kill -${1:-9}
  fi
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
alias magit='emacsclient -nw -e "(magit-status)"'

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

[[ -f /usr/bin/thefuck ]] && eval $(thefuck --alias fck)

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/krikava/.sdkman"
[[ -s "/home/krikava/.sdkman/bin/sdkman-init.sh" ]] && source "/home/krikava/.sdkman/bin/sdkman-init.sh"

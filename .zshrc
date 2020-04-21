# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block, everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

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

function cdu {
  local declare dirs=()
  get_parent_dirs() {
    if [[ -d "${1}" ]]; then dirs+=("$1"); else return; fi
    if [[ "${1}" == '/' ]]; then
      for _dir in "${dirs[@]}"; do echo $_dir; done
    else
      get_parent_dirs $(dirname "$1")
    fi
  }
  local DIR=$(get_parent_dirs $(realpath "${1:-$PWD}") | fzf-tmux --tac)
  cd "$DIR"
}

z() {
    dir="$(fasd -Rdl "$*" | fzf -1 -0 --no-sort +m)" && cd "${dir}" || return 1
}

fman() {
    man -k . | fzf --prompt='> ' | awk '{print $1}' | xargs -r man
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
alias j=z
command -v exa >/dev/null 2>&1 && alias ls='exa --group-directories-first --color=auto'
command -v bat >/dev/null 2>&1 && alias cat=bat

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
[[ -f /usr/share/fzf/key-bindings.zsh ]] && source /usr/share/fzf/key-bindings.zsh
[[ -f /usr/share/fzf/completion.zsh ]] && source /usr/share/fzf/completion.zsh
[[ -f ~/.fzf.zsh ]] && source ~/.fzf.zsh
export FZF_DEFAULT_OPTS='
  --color fg:#D8DEE9,bg:#2E3440,hl:#A3BE8C,fg+:#D8DEE9,bg+:#434C5E,hl+:#A3BE8C
  --color pointer:#BF616A,info:#4C566A,spinner:#4C566A,header:#4C566A,prompt:#81A1C1,marker:#EBCB8B
'

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

# should be done by zprezto
# [[ -f ~/.dir_colors ]] && eval $(dircolors ~/.dir_colors)

[[ -f /usr/bin/thefuck ]] && eval $(thefuck --alias fck)

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/krikava/.sdkman"
[[ -s "/home/krikava/.sdkman/bin/sdkman-init.sh" ]] && source "/home/krikava/.sdkman/bin/sdkman-init.sh"

# git extras
[[ -f ~/.local/share/zsh/git-extras-completion.zsh ]] && source ~/.local/share/zsh/git-extras-completion.zsh

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

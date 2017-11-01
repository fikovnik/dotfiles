################################################################################
## Functions
################################################################################

function config_osx {
  # iTerm2
  [[ -e ${HOME}/.iterm2_shell_integration.zsh ]] && source ${HOME}/.iterm2_shell_integration.zsh

  alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
}

function config_linux {
  [[ -d /usr/lib/jvm/java ]] && export JAVA_HOME=/usr/lib/jvm/java
}

################################################################################

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

export DEFAULT_USER=krikava

# language
export LC_ALL=en_US.UTF-8

# aliases
alias g="emacsclient -t"
alias vi=vim

# LESS
export LESS='-F -g -i -M -R -S -w -X -z-4'

# dot files
alias config='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

# ZSH options

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

# autoload
autoload -U zmv

# vcs_info
VCS_BRANCH_ICON=""
VCS_REMOTE_BRANCH_ICON=""
zstyle ':vcs_info:git:*' formats "%F{$VCS_FOREGROUND_COLOR}%s:%b%f"
zstyle ':vcs_info:git-svn:*' formats "%F{$VCS_FOREGROUND_COLOR}%s:%b%f"
zstyle ':vcs_info:hg:*' formats "%F{$VCS_FOREGROUND_COLOR}%s:%b%f"

if [ -z "$SSH_CONNECTION" ] || emacsclient --version >/dev/null 2>&1; then
    alias g="emacsclient -t"

    export ALTERNATE_EDITOR="vim"
    export EDITOR="$(which emacsclient) -t"
    export VISUAL=$EDITOR
else
    alias g="vim"

    export EDITOR="vim"
    export VISUAL=$EDITOR
fi

alias emacs='emacs -nw'

# completion
[[ -d /usr/local/share/zsh-completions ]] && fpath=(/usr/local/share/zsh-completions $fpath)
[[ -d $HOME/.zsh/completions ]] && fpath=($HOME/.zsh/completions $fpath)
#fpath=("/usr/local/bin/" $fpath)
#autoload -Uz compinit 
compinit -u

# jenv
if [[ -f ~/.jenv/bin/jenv ]]; then
  export PATH="$HOME/.jenv/bin:$PATH"
  eval "$(jenv init -)"
fi

# pyenv
[[ -f /usr/local/bin/pyenv ]] && eval "$(pyenv init -)"

case $(uname) in
  Darwin*) config_osx ;;
  Linux*) config_linux ;;
esac

# custom keys
bindkey '^[z' undo

# azure
type azure >/dev/null 2>&1 && . <(azure --completion)

# fasd
if which fasd > /dev/null; then
  # fasd
  fasd_cache="$HOME/.fasd-init-zsh"
  if [ "$(command -v fasd)" -nt "$fasd_cache" -o ! -s "$fasd_cache" ]; then
    fasd --init auto >| "$fasd_cache"
  fi
  source "$fasd_cache"
  unset fasd_cache
  alias v='f -e vim'
  alias j='fasd_cd -d'
  alias jj='fasd_cd -d -i'
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

bindkey '\e[1;3D' backward-word
bindkey '\e[1;3C' forward-word
bindkey '\e\eOD' backward-word
bindkey '\e\eOC' forward-word

export PATH="/usr/local/bin:/usr/local/sbin:$PATH"

################################################################################
## Functions
################################################################################

function config_osx {
  # iTerm2
  [[ -e ${HOME}/.iterm2_shell_integration.zsh ]] && source ${HOME}/.iterm2_shell_integration.zsh

  alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
}

function config_linux {

}

################################################################################

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
# fix powerline
  if [[ ! -L ~/.zprezto/modules/prompt/functions/prompt_powerlevel9k_setup ]]; then
    ln -sf ~/.zprezto/modules/prompt/external/powerlevel9k/powerlevel9k.zsh-theme ~/.zprezto/modules/prompt/functions/prompt_powerlevel9k_setup
  fi

  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

export DEFAULT_USER=krikava

# language
export LC_ALL=en_US.UTF-8

# aliases
alias g="emacs"

# LESS
export LESS='-F -g -i -M -R -S -w -X -z-4'

# dot files
alias config='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

# ZSH options

# help
unalias run-help
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

if [ -z "$SSH_CONNECTION" ]; then
    alias g="emacsclient -n -c"
    
    export ALTERNATE_EDITOR=""
    export EDITOR="emacsclient -c"
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

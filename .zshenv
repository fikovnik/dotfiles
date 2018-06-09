# jenv
if [[ -f ~/.jenv/bin/jenv ]]; then
  export PATH="$HOME/.jenv/bin:$PATH"
  eval "$(jenv init -)"
fi

export PATH="/usr/local/bin:/usr/local/sbin:$PATH"

[ -d ~/.local/bin ] && export PATH="~/.local/bin:$PATH"

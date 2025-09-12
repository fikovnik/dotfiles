#!/usr/bin/env bash
set -euo pipefail

have() { command -v "$1" >/dev/null 2>&1; }
log() { printf "\n\033[1m==> %s\033[0m\n" "$*"; }

install_linux_base() {
  if ! have sudo; then
    echo "sudo is required on Linux" >&2
    exit 1
  fi
  log "Updating apt index (non-interactive)"
  DEBIAN_FRONTEND=noninteractive sudo apt-get -yq update

  local pkgs=(
    bfs
    git-delta
    ripgrep
    tmux
  )
  log "Installing APT packages: ${pkgs[*]}"
  DEBIAN_FRONTEND=noninteractive sudo apt-get -yq install "${pkgs[@]}"
}

install_macos_base() {
  if ! have brew; then
    log "Installing Homebrew (non-interactive)"
    NONINTERACTIVE=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  fi

  eval "$(/opt/homebrew/bin/brew shellenv)"

  local pkgs=(
    bfs
    git-delta
    ripgrep
    tmux
  )
  log "Updating Homebrew"
  brew update --quiet

  log "Installing/Upgrading Homebrew packages: ${pkgs[*]}"
  brew install --quiet "${pkgs[@]}" || true
  brew upgrade --quiet "${pkgs[@]}" || true
}

install_with_mise() {
  export PATH="$HOME/.local/bin:$PATH"

  if ! have mise; then
    log "Installing mise"
    curl -fsSL https://mise.run | sh
  fi

  eval "$(mise activate bash)"

  local tools=(
    fd@latest
    fzf@latest
    lazygit@latest
    neovim@latest
    node@latest
    tokei@latest
  )

  log "Installing with mise: ${tools[*]}"
  for t in "${tools[@]}"; do
    mise use -g "$t"
  done
}

case "$(uname -s)" in
Linux*)
  install_linux_base
  ;;
Darwin*)
  install_macos_base
  ;;
*)
  echo "Unsupported OS: $(uname -s)" >&2
  exit 1
  ;;
esac

install_with_mise

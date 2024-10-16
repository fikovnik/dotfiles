#!/bin/bash

BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

install_on_linux() {
  local nix_profile="$HOME/.nix-profile/etc/profile.d/nix.sh"
  if [[ -f "$nix_profile" ]]; then
    echo 'Nix is already installed'
    . "$nix_profile"
  else
    sh <(curl -L https://nixos.org/nix/install) --no-daemon
    . "$nix_profile"
    nix-channel --add https://nixos.org/channels/nixos-24.05 nixpkgs
    nix-channel --update
  fi
  nix-env -if "$BASE_DIR/packages.nix"
}

install_on_mac() {
  xcode-select --install || echo "XCode already installed"
  if which brew; then
    echo 'Homebrew is already installed'
  else
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  fi
}

OS="$(uname -s)"
case "${OS}" in
Linux*)
  install_on_linux
  ;;
Darwin*)
  install_on_mac
  ;;
*)
  echo "Unsupported operating system: ${OS}"
  exit 1
  ;;
esac

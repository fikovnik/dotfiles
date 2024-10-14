#!/bin/bash

install_on_linux() {
  if which nix; then
    echo 'Nix is already installed'
  else
    sh <(curl -L https://nixos.org/nix/install) --no-daemon
  fi
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

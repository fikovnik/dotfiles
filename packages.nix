{ pkgs ? import <nixpkgs> {} }:

{
  inherit (pkgs)
  bfs
  cloc
  fzf
  delta
  lazygit
  neovim
  nodejs
  ripgrep
  tmux;
}

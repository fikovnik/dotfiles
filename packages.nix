{ pkgs ? import <nixpkgs> {},
  unstable ? import <unstable> { } }:

{
  inherit (pkgs)
    bfs
    cloc
    clang-tools
    fzf
    delta
    lazygit
    nodejs
    ripgrep
    tmux;

  neovim = unstable.neovim;
}

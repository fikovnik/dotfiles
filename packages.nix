{ unstable ? import <unstable> {} }:

{
  inherit (unstable)
    bfs
    cloc
    clang-tools
    fzf
    delta
    lazygit
    neovim
    nodejs
    ripgrep
    tmux;
}

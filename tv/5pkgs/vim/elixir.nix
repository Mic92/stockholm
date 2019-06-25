{ pkgs }:
pkgs.vimUtils.buildVimPlugin {
  name = "vim-elixir-2018-08-17";
  src = pkgs.fetchgit {
    url = https://github.com/elixir-editors/vim-elixir;
    rev = "0a847f0faed5ba2d94bb3d51f355c50f37ba025b";
    sha256 = "1jl85wpgywhcvhgw02y8zpvqf0glr4i8522kxpvhsiacb1v1xh04";
  };
}

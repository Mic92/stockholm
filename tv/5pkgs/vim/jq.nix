{ pkgs }:

pkgs.vimUtils.buildVimPlugin {
 name = "vim-syntax-jq";
 src = pkgs.fetchgit {
   url = https://github.com/vito-c/jq.vim;
   rev = "99d55a300047946a82ecdd7617323a751199ad2d";
   sha256 = "09c94nah47wx0cr556w61h6pfznxld18pfblc3nv51ivbw7cjqyx";
 };
}

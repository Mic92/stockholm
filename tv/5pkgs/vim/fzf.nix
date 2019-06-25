{ pkgs }:

# cannot use pkgs.vimPlugins.fzf-vim as it's missing :Rg
pkgs.vimUtils.buildVimPlugin {
  name = "fzf-2018-11-14";
  src = pkgs.fetchgit {
    url = https://github.com/junegunn/fzf.vim;
    rev = "ad1833ecbc9153b6e34a4292dc089a58c4bcb8dc";
    sha256 = "1z2q71q6l9hq9fqfqpj1svhyk4yk1bzw1ljhksx4bnpz8gkfbx2m";
  };
}

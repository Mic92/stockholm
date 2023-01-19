{ pkgs }:

# cannot use pkgs.vimPlugins.fzf-vim as it's missing :Rg
pkgs.vimUtils.buildVimPlugin {
  name = "fzf-2023-01-16";
  src = pkgs.fetchgit {
    url = https://github.com/junegunn/fzf.vim;
    rev = "bdf48c282ad2174c25c059b3cdb7956427b07a99";
    hash = "sha256-eCCk+Q596Ljjdtjd0cYGqR77K3Me5gf+ts5icP22S3Y=";
  };
}

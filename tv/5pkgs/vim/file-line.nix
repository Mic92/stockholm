{ pkgs }:

pkgs.vimUtils.buildVimPlugin {
  name = "file-line-1.0";
  src = pkgs.fetchgit {
    url = https://github.com/bogado/file-line;
    rev = "refs/tags/1.0";
    sha256 = "0z47zq9rqh06ny0q8lpcdsraf3lyzn9xvb59nywnarf3nxrk6hx0";
  };
}

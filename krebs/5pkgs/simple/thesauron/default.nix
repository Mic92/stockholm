{ fetchgit, callPackage }: let
  src = fetchgit {
    url = "https://github.com/krebscode/thesauron";
    rev = "8ac22588cf2c20465e3c9348e7ce04885599c2a5";
    "sha256"= "1ivkjl235dnm5aaqqvarnxkz7zh0gvah22b0fqwlsflrcd5wmgva";
  };
in callPackage src {}

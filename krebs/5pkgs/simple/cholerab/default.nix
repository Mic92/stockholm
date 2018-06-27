{ fetchgit, callPackage }: let
  src = fetchgit {
    url = "https://github.com/krebs/cholerab";
    rev = "403107219ba9f3b6881f2cdae57ac373a13d98f0";
    sha256 = "076giaha52zxkvkr2f471g2rl9c5m8r5g03wncgh46qmdfcb5idb";
  };
in callPackage src {}

{ fetchgit, callPackage }: let
  src = fetchgit {
    url = "https://github.com/krebs/cholerab";
    rev = "25d7ef051d6fc74d99b155e768b3c650296a230c";
    sha256 = "1pymw7v2ql42iq825ccx98s4fp9jsz5b2hjr1qad6bamfc6i7yy9";
  };
in callPackage src {}

with import <stockholm/lib>;
host@{ name, secure ? false }: let
  builder = if getEnv "dummy_secrets" == "true"
              then "buildbot"
              else "tv";
  _file = <stockholm> + "/tv/1systems/${name}/source.nix";
in
  evalSource (toString _file) {
    nixos-config.symlink = "stockholm/tv/1systems/${name}/config.nix";
    secrets.file = getAttr builder {
      buildbot = toString <stockholm/tv/dummy_secrets>;
      tv = "/home/tv/secrets/${name}";
    };
    stockholm.file = toString <stockholm>;
    secrets-common.file = "/home/tv/secrets/common";
    nixpkgs.git = {
      url = https://github.com/NixOS/nixpkgs;
      ref = "1b57bf274ae5c76e91b2b264d8aa8bfcecb72102"; # nixos-17.03
    };
  } // optionalAttrs secure {
    secrets-master.file = "/home/tv/secrets/master";
  }

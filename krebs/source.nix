with import <stockholm/lib>;
host@{ name, secure ? false }: let
  builder = if getEnv "dummy_secrets" == "true"
              then "buildbot"
              else "krebs";
  _file = <stockholm> + "/krebs/1systems/${name}/source.nix";
in
  evalSource (toString _file) {
    nixos-config.symlink = "stockholm/krebs/1systems/${name}/config.nix";
    secrets = getAttr builder {
      buildbot.file = toString <stockholm/krebs/6tests/data/secrets>;
      krebs.pass = {
        dir = "${getEnv "HOME"}/brain";
        name = "krebs-secrets/${name}";
      };
    };
    stockholm.file = toString <stockholm>;
    nixpkgs.git = {
      url = https://github.com/NixOS/nixpkgs;
      ref = "cb751f9b1c3fe6885f3257e69ce328f77523ad77"; # nixos-17.09 @ 2017-12-13
    };
  }

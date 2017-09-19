with import <stockholm/lib>;
host@{ name, secure ? false }: let
  builder = if getEnv "dummy_secrets" == "true"
              then "buildbot"
              else "krebs";
  _file = <stockholm> + "/krebs/1systems/${name}/source.nix";
in
  evalSource (toString _file) {
    nixos-config.symlink = "stockholm/krebs/1systems/${name}/config.nix";
    secrets.file = getAttr builder {
      buildbot = toString <stockholm/krebs/6tests/data/secrets>;
      krebs = "${getEnv "HOME"}/secrets/krebs/${host.name}";
    };
    stockholm.file = toString <stockholm>;
    nixpkgs.git = {
      url = https://github.com/NixOS/nixpkgs;
      ref = "670b4e29adc16e0a29aa5b4c126703dcca56aeb6"; # nixos-17.09 @ 2017-09-18
    };
  }

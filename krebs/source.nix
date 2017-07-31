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
      ref = "0590ecbe9e6b9a076065be29370701da758c61f1"; # nixos-17.03 @ 2017-07-30
    };
  }

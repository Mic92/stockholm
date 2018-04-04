with import <stockholm/lib>;
host@{ name, secure ? false, override ? {} }: let
  builder = if getEnv "dummy_secrets" == "true"
              then "buildbot"
              else "krebs";
  _file = <stockholm> + "/krebs/1systems/${name}/source.nix";
  pkgs = import <nixpkgs> {
    overlays = map import [
      <stockholm/krebs/5pkgs>
    ];
  };
in
  evalSource (toString _file) [
    {
      nixos-config.symlink = "stockholm/krebs/1systems/${name}/config.nix";
      secrets = getAttr builder {
        buildbot.file = toString <stockholm/krebs/6tests/data/secrets>;
        krebs.pass = {
          dir = "${getEnv "HOME"}/brain";
          name = "krebs-secrets/${name}";
        };
      };
      stockholm.file = toString <stockholm>;
      stockholm-version.pipe = "${pkgs.stockholm}/bin/get-version";
      nixpkgs.git = {
        url = https://github.com/NixOS/nixpkgs;
        ref = "b6ddb9913f2b8206837e0f137db907bdefb9275e"; # nixos-18.03 # 2018-03-24
      };
    }
    override
  ]

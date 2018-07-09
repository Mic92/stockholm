with import <stockholm/lib>;
host@{ name, secure ? false, override ? {} }: let
  builder = if getEnv "dummy_secrets" == "true"
              then "buildbot"
              else "krebs";
  _file = <stockholm> + "/krebs/1systems/${name}/source.nix";
  pkgs = import <nixpkgs> {
    overlays = map import [
      <stockholm/krebs/5pkgs>
      <stockholm/submodules/nix-writers/pkgs>
    ];
  };
in
  evalSource (toString _file) [
    {
      nixos-config.symlink = "stockholm/krebs/1systems/${name}/config.nix";
      secrets = getAttr builder {
        buildbot.file = toString <stockholm/krebs/0tests/data/secrets>;
        krebs.pass = {
          dir = "${getEnv "HOME"}/brain";
          name = "krebs-secrets/${name}";
        };
      };
      stockholm.file = toString <stockholm>;
      stockholm-version.pipe = "${pkgs.stockholm}/bin/get-version";
      nixpkgs = (import ./krops.nix { name = ""; }).krebs-source.nixpkgs;
    }
    override
  ]

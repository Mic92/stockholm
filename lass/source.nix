with import <stockholm/lib>;
host@{ name, secure ? false, override ? {} }: let
  builder = if getEnv "dummy_secrets" == "true"
              then "buildbot"
              else "lass";
  _file = <stockholm> + "/lass/1systems/${name}/source.nix";
  pkgs = import <nixpkgs> {
    overlays = map import [
      <stockholm/krebs/5pkgs>
      <stockholm/submodules/nix-writers/pkgs>
    ];
  };
in
  evalSource (toString _file) [
    {
      nixos-config.symlink = "stockholm/lass/1systems/${name}/physical.nix";
      nixpkgs = (import <stockholm/krebs/source.nix> host).nixpkgs;
      secrets = getAttr builder {
        buildbot.file = toString <stockholm/lass/2configs/tests/dummy-secrets>;
        lass.pass = {
          dir = "${getEnv "HOME"}/.password-store";
          name = "hosts/${name}";
        };
      };
      stockholm.file = toString <stockholm>;
      stockholm-version.pipe = "${pkgs.stockholm}/bin/get-version";
    }
    override
  ]

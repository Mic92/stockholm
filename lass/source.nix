with import <stockholm/lib>;
host@{ name, secure ? false, override ? {} }: let
  builder = if getEnv "dummy_secrets" == "true"
              then "buildbot"
              else "lass";
  _file = <stockholm> + "/lass/1systems/${name}/source.nix";
in
  evalSource (toString _file) [
    {
      nixos-config.symlink = "stockholm/lass/1systems/${name}/config.nix";
      nixpkgs.git = {
        url = https://github.com/nixos/nixpkgs;
        ref = "b61d084";
      };
      secrets.file = getAttr builder {
        buildbot = toString <stockholm/lass/2configs/tests/dummy-secrets>;
        lass = "/home/lass/secrets/${name}";
      };
      stockholm.file = toString <stockholm>;
    }
    override
  ]

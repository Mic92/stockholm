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
        url = http://cgit.lassul.us/nixpkgs;
        # nixos-17.03
        # + copytoram:
        #   87a4615 & 334ac4f
        # + acme permissions for groups
        #   fd7a8f1
        ref = "2d3b4fe";
      };
      secrets.file = getAttr builder {
        buildbot = toString <stockholm/lass/2configs/tests/dummy-secrets>;
        lass = "/home/lass/secrets/${name}";
      };
      stockholm.file = toString <stockholm>;
    }
    override
  ]

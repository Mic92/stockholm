with import <stockholm/lib>;
host@{ name, override ? {} }: let
  builder = if getEnv "dummy_secrets" == "true"
              then "buildbot"
              else "mv";
  _file = <stockholm> + "/mv/1systems/${name}/source.nix";
in
  evalSource (toString _file) [
    {
      nixos-config.symlink = "stockholm/mv/1systems/${name}/config.nix";
      nixpkgs.git = {
        # nixos-17.09
        ref = mkDefault "d0f0657ca06cc8cb239cb94f430b53bcdf755887";
        url = https://github.com/NixOS/nixpkgs;
      };
      secrets.file = getAttr builder {
        buildbot = toString <stockholm/mv/dummy_secrets>;
        mv = "/home/mv/secrets/${name}";
      };
      stockholm.file = toString <stockholm>;
    }
    override
  ]

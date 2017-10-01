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
        # nixos-17.03
        ref = mkDefault "3d04a557b72aa0987d9bf079e1445280b6bfd907";
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

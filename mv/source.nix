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
        ref = mkDefault "56da88a298a6f549701a10bb12072804a1ebfbd5";
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

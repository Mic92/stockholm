with import <stockholm/lib>;
{ name
, dummy_secrets ? getEnv "dummy_secrets" == "true"
, override ? {}
, secure ? false
}@host: let
  builder = if dummy_secrets then "buildbot" else "tv";
  _file = <stockholm> + "/tv/1systems/${name}/source.nix";
in
  evalSource (toString _file) [
    {
      nixos-config.symlink = "stockholm/tv/1systems/${name}/config.nix";
      nixpkgs.git = {
        # nixos-17.09
        ref = mkDefault "53e6d671a9662922080635482b7e1c418d2cdc72";
        url = https://github.com/NixOS/nixpkgs;
      };
      secrets.file = getAttr builder {
        buildbot = toString <stockholm/tv/dummy_secrets>;
        tv = "/home/tv/secrets/${name}";
      };
      stockholm.file = toString <stockholm>;
    }
    (mkIf (builder == "tv") {
      secrets-common.file = "/home/tv/secrets/common";
    })
    (mkIf (builder == "tv" && secure) {
      secrets-master.file = "/home/tv/secrets/master";
    })
    override
  ]

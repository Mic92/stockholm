with import <stockholm/lib>;
{ name
, dummy_secrets ? getEnv "dummy_secrets" == "true"
, override ? {}
, secure ? false
}@host: let
  builder = if dummy_secrets then "buildbot" else "tv";
  _file = <stockholm> + "/tv/1systems/${name}/source.nix";
  pkgs = import <nixpkgs> {
    overlays = map import [
      <stockholm/krebs/5pkgs>
    ];
  };
in
  evalSource (toString _file) [
    {
      nixos-config.symlink = "stockholm/tv/1systems/${name}/config.nix";
      nixpkgs.git = {
        ref = mkDefault "7cbf6ca1c84dfc917c1a99524e082fb677501844";
        url = https://github.com/NixOS/nixpkgs;
      };
      secrets.file = getAttr builder {
        buildbot = toString <stockholm/tv/dummy_secrets>;
        tv = "/home/tv/secrets/${name}";
      };
      stockholm.file = toString <stockholm>;
      stockholm-version.pipe = "${pkgs.stockholm}/bin/get-version";
    }
    (mkIf (builder == "tv") {
      secrets-common.file = "/home/tv/secrets/common";
    })
    (mkIf (builder == "tv" && secure) {
      secrets-master.file = "/home/tv/secrets/master";
    })
    override
  ]

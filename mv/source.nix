with import <stockholm/lib>;
host@{ name, override ? {} }: let
  builder = if getEnv "dummy_secrets" == "true"
              then "buildbot"
              else "mv";
  _file = <stockholm> + "/mv/1systems/${name}/source.nix";
  pkgs = import <nixpkgs> {
    overlays = map import [
      <stockholm/krebs/5pkgs>
    ];
  };
in
  evalSource (toString _file) [
    {
      nixos-config.symlink = "stockholm/mv/1systems/${name}/config.nix";
      nixpkgs.git = {
        # nixos-17.09
        ref = mkDefault "0653b73bf61f3a23d28c38ab7e9c69a318d433de";
        url = https://github.com/NixOS/nixpkgs;
      };
      secrets.file = getAttr builder {
        buildbot = toString <stockholm/mv/dummy_secrets>;
        mv = "/home/mv/secrets/${name}";
      };
      stockholm.file = toString <stockholm>;
      stockholm-version.pipe = "${pkgs.stockholm}/bin/get-version";
    }
    override
  ]

with import <stockholm/lib>;
host@{ name, secure ? false, override ? {} }: let
  builder = if getEnv "dummy_secrets" == "true"
              then "buildbot"
              else "jeschli";
  _file = <stockholm> + "/jeschli/1systems/${name}/source.nix";
  pkgs = import <nixpkgs> {
    overlays = map import [
      <stockholm/krebs/5pkgs>
    ];
  };
in
  evalSource (toString _file) [
    {
      nixos-config.symlink = "stockholm/jeschli/1systems/${name}/config.nix";
      nixpkgs = (import <stockholm/krebs/source.nix> host).nixpkgs;
      secrets.file = getAttr builder {
        buildbot = toString <stockholm/jeschli/2configs/tests/dummy-secrets>;
        jeschli = "${getEnv "HOME"}/secrets/${name}";
      };
      stockholm.file = toString <stockholm>;
      stockholm-version.pipe = "${pkgs.stockholm}/bin/get-version";
    }
    override
  ]

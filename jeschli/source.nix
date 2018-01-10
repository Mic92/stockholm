with import <stockholm/lib>;
host@{ name, secure ? false, override ? {} }: let
  builder = if getEnv "dummy_secrets" == "true"
              then "buildbot"
              else "jeschli";
  _file = <stockholm> + "/jeschli/1systems/${name}/source.nix";
in
  evalSource (toString _file) [
    {
      nixos-config.symlink = "stockholm/jeschli/1systems/${name}/config.nix";
      nixpkgs.git = {
        url = https://github.com/nixos/nixpkgs;
        ref = "0653b73";
      };
      secrets.file = getAttr builder {
        buildbot = toString <stockholm/jeschli/2configs/tests/dummy-secrets>;
        jeschli = "${getEnv "HOME"}/secrets/${name}";
      };
      stockholm.file = toString <stockholm>;
    }
    override
  ]

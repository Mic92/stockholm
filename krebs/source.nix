with import <stockholm/lib>;
host@{ name, secure ? false, override ? {} }: let
  builder = if getEnv "dummy_secrets" == "true"
              then "buildbot"
              else "krebs";
  _file = <stockholm> + "/krebs/1systems/${name}/source.nix";
in
  evalSource (toString _file) [
    {
      nixos-config.symlink = "stockholm/krebs/1systems/${name}/config.nix";
      secrets = getAttr builder {
        buildbot.file = toString <stockholm/krebs/6tests/data/secrets>;
        krebs.pass = {
          dir = "${getEnv "HOME"}/brain";
          name = "krebs-secrets/${name}";
        };
      };
      stockholm.file = toString <stockholm>;
      nixpkgs.git = {
        url = https://github.com/NixOS/nixpkgs;
        ref = "d09e425aea3e09b6cec5c7b05cc0603f6853748b"; # nixos-17.09 @ 2018-02-22
      };
    }
    override
  ]

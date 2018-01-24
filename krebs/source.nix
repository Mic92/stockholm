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
        ref = "b222a0713f4a689519686421b3fbf5ca0c9bd3da"; # nixos-17.09 @ 2018-01-23
      };
    }
    override
  ]

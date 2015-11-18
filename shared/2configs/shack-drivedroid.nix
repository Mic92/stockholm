{ pkgs, lib, config, ... }:
let
  repodir = "/var/srv/drivedroid";
  srepodir = lib.shell.escape repodir;
in
{
  systemd.paths.drivedroid = {
    wantedBy = [ "multi-user.target" ];
    Description = "triggers for changes in drivedroid dir";
    pathConfig = {
      PathModified = repodir;
    };
  };

  systemd.services.drivedroid = {
    ServiceConfig = {
      ExecStartPre = pkgs.writeScript "prepare-drivedroid-repo-gen" ''
        #!/bin/sh
        mkdir -p ${srepodir}/repos
      '';
      ExecStart = pkgs.writeScript "start-drivedroid-repo-gen" ''
        #!/bin/sh
        {pkgs.drivedroid-gen-repo}/bin/drivedroid-gen-repo --chdir "${srepodir}" repos/ > "${srepodir}/main.json"
      '';
    };
  };

  krebs.nginx = {
    enable = lib.mkDefault true;
    servers = {
      drivedroid-repo = {
        server-names = [ "drivedroid.shack" ];
        # TODO: prepare this somehow
        locations = lib.singleton (lib.nameValuePair "/" ''
          root ${repodir};
          index main.json;
        '');
      };
    };
  };

}

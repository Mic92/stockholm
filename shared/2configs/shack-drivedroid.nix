{ pkgs, lib, config, ... }:

let
  repodir = "/var/srv/drivedroid";
  srepodir = config.krebs.lib.shell.escape repodir;
in
{
  environment.systemPackages = [ pkgs.drivedroid-gen-repo ];

  systemd.services.drivedroid = {
    description = "generates drivedroid repo file";
    restartIfChanged = true;
    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      Type = "simple";
      Restart = "always";
      ExecStartPre = pkgs.writeScript "prepare-drivedroid-gen-repo" ''
        #!/bin/sh
        mkdir -p ${srepodir}/repos
      '';
      ExecStart = pkgs.writeScript "start-drivedroid-gen-repo" ''
        #!/bin/sh
        while sleep 60; do
          ${pkgs.inotify-tools}/bin/inotifywait -r ${srepodir} && ${pkgs.drivedroid-gen-repo}/bin/drivedroid-gen-repo --chdir "${srepodir}" repos/ > "${srepodir}/main.json"
        done
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

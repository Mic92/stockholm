{ config, pkgs, ... }:
with import <stockholm/lib>;
let
  repodir = "/var/srv/drivedroid";
  srepodir = shell.escape repodir;
in
{
  environment.systemPackages = [ pkgs.drivedroid-gen-repo ];

  services.nginx = {
    enable = mkDefault true;
    virtualHosts.shack-drivedroid = {
      serverAliases = [
        "drivedroid.shack"
      ];
      # TODO: prepare this somehow
      locations."/".extraConfig = ''
        root ${repodir};
        index main.json;
      '';
    };
  };

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
}

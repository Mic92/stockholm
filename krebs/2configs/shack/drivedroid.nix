{ config, lib, pkgs, ... }:
with import ../../../lib/pure.nix { inherit lib; };
let
  root = "/var/srv/drivedroid";
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
        root ${root};
        index main.json;
      '';
    };
  };

  systemd.services.drivedroid-gen-repo = {
    description = "generates drivedroid repo file";
    path = [
      pkgs.coreutils
      pkgs.drivedroid-gen-repo
      pkgs.inotify-tools
    ];
    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      Type = "simple";
      Restart = "always";
      ExecStartPre = pkgs.writeDash "prepare-drivedroid-gen-repo" ''
        mkdir -p ${root}/repos
      '';
      ExecStart = pkgs.writeDash "start-drivedroid-gen-repo" ''
        set -efu
        cd ${root}
        while sleep 60; do
          if inotifywait -r .; then
            drivedroid-gen-repo repos > main.json
          fi
        done
      '';
    };
  };
}

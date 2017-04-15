{ config, lib, pkgs, ... }:

with import <stockholm/lib>;
let
  cfg = config.krebs.hidden-ssh;

  out = {
    options.krebs.hidden-ssh = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "hidden SSH announce";
  };

  imp = let
    torDirectory = "/var/lib/tor"; # from tor.nix
    hiddenServiceDir = torDirectory + "/ssh-announce-service";
  in {
    services.tor = {
      enable = true;
      extraConfig = ''
        HiddenServiceDir ${hiddenServiceDir}
        HiddenServicePort 22 127.0.0.1:22
      '';
      client.enable = true;
    };
    systemd.services.hidden-ssh-announce = {
      description = "irc announce hidden ssh";
      after = [ "tor.service" ];
      wants = [ "tor.service" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        # ${pkgs.tor}/bin/torify
        ExecStart = pkgs.writeDash "irc-announce-ssh" ''
          set -efu
          until test -e ${hiddenServiceDir}/hostname; do
            echo "still waiting for ${hiddenServiceDir}/hostname"
            sleep 1
          done
          ${pkgs.irc-announce}/bin/irc-announce \
            irc.freenode.org 6667 ${config.krebs.build.host.name}-ssh \
            \#krebs-announce \
            "SSH Hidden Service at $(cat ${hiddenServiceDir}/hostname)"
        '';
        PrivateTmp = "true";
        User = "tor";
        Type = "oneshot";
      };
    };
  };
in
out

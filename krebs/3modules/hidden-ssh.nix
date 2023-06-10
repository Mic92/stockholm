{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.krebs.hidden-ssh;

  out = {
    options.krebs.hidden-ssh = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "hidden SSH announce";
    channel = mkOption {
      type = types.str;
      default = "#krebs-announce";
    };
    server = mkOption {
      type = types.str;
      default = "irc.hackint.org";
    };
    port = mkOption {
      type = types.int;
      default = 6697;
    };
    tls = mkOption {
      type = types.bool;
      default = true;
    };
    message = mkOption {
      type = types.str;
      default = "SSH Hidden Service at ";
    };
  };

  imp = let
    torDirectory = "/var/lib/tor"; # from tor.nix
    hiddenServiceDir = torDirectory + "/onion/hidden-ssh";
  in {
    services.tor = {
      enable = true;
      relay.onionServices.hidden-ssh = {
        version = 3;
        map = [{
          port = 22;
          target.port = 22;
        }];
      };
      client.enable = true;
    };
    systemd.services.hidden-ssh-announce = {
      description = "irc announce hidden ssh";
      after = [ "tor.service" "network-online.target" ];
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
          ${pkgs.untilport}/bin/untilport ${escapeShellArg cfg.server} ${toString cfg.port}

          ${pkgs.irc-announce}/bin/irc-announce \
            ${escapeShellArg cfg.server} \
            ${toString cfg.port} \
            "${config.krebs.build.host.name}-ssh" \
            ${escapeShellArg cfg.channel} \
            ${escapeShellArg cfg.tls} \
            "${cfg.message}$(cat ${hiddenServiceDir}/hostname)"
        '';
        PrivateTmp = "true";
        User = "tor";
        Type = "oneshot";
      };
    };
  };
in
out

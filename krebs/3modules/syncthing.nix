{ config, pkgs, ... }: with import <stockholm/lib>;

let

  cfg = config.krebs.syncthing;

  devices = mapAttrsToList (name: peer: {
    name = name;
    deviceID = peer.id;
    addresses = peer.addresses;
  }) cfg.peers;

  folders = mapAttrsToList ( _: folder: {
    inherit (folder) path id type;
    devices = map (peer: { deviceId = cfg.peers.${peer}.id; }) folder.peers;
    rescanIntervalS = folder.rescanInterval;
    fsWatcherEnabled = folder.watch;
    fsWatcherDelayS = folder.watchDelay;
    ignorePerms = folder.ignorePerms;
  }) cfg.folders;

  getApiKey = pkgs.writeDash "getAPIKey" ''
    ${pkgs.libxml2}/bin/xmllint \
      --xpath 'string(configuration/gui/apikey)'\
      ${config.services.syncthing.configDir}/config.xml
  '';

  updateConfig = pkgs.writeDash "merge-syncthing-config" ''
    set -efu
    # wait for service to restart
    ${pkgs.untilport}/bin/untilport localhost 8384
    API_KEY=$(${getApiKey})
    CFG=$(${pkgs.curl}/bin/curl -Ss -H "X-API-Key: $API_KEY" localhost:8384/rest/system/config)
    echo "$CFG" | ${pkgs.jq}/bin/jq -s '.[] as $in | $in * {
      "devices": (${builtins.toJSON devices}${optionalString (! cfg.overridePeers) " + $in.devices"}),
      "folders": (${builtins.toJSON folders}${optionalString (! cfg.overrideFolders) " + $in.folders"})
    }' | ${pkgs.curl}/bin/curl -Ss -H "X-API-Key: $API_KEY" localhost:8384/rest/system/config -d @-
    ${pkgs.curl}/bin/curl -Ss -H "X-API-Key: $API_KEY" localhost:8384/rest/system/restart -X POST
  '';

in

{
  options.krebs.syncthing = {

    enable = mkEnableOption "syncthing-init";

    cert = mkOption {
      type = types.nullOr types.absolute-pathname;
      default = null;
    };

    key = mkOption {
      type = types.nullOr types.absolute-pathname;
      default = null;
    };

    overridePeers = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether to delete the peers which are not configured via the peers option
      '';
    };
    peers = mkOption {
      default = {};
      type = types.attrsOf (types.submodule ({
        options = {

          # TODO make into addr + port submodule
          addresses = mkOption {
            type = types.listOf types.str;
            default = [];
          };

          #TODO check
          id = mkOption {
            type = types.str;
          };

        };
      }));
    };

    overrideFolders = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether to delete the folders which are not configured via the peers option
      '';
    };
    folders = mkOption {
      default = {};
      type = types.attrsOf (types.submodule ({ config, ... }: {
        options = {

          path = mkOption {
            type = types.absolute-pathname;
            default = config._module.args.name;
          };

          id = mkOption {
            type = types.str;
            default = config._module.args.name;
          };

          peers = mkOption {
            type = types.listOf types.str;
            default = [];
          };

          rescanInterval = mkOption {
            type = types.int;
            default = 3600;
          };

          type = mkOption {
            type = types.enum [ "sendreceive" "sendonly" "receiveonly" ];
            default = "sendreceive";
          };

          watch = mkOption {
            type = types.bool;
            default = true;
          };

          watchDelay = mkOption {
            type = types.int;
            default = 10;
          };

          ignorePerms = mkOption {
            type = types.bool;
            default = true;
          };

        };
      }));
    };
  };

  config = (mkIf cfg.enable) {

    systemd.services.syncthing = mkIf (cfg.cert != null || cfg.key != null) {
      preStart = ''
        ${optionalString (cfg.cert != null) ''
          cp ${toString cfg.cert} ${config.services.syncthing.configDir}/cert.pem
          chown ${config.services.syncthing.user}:${config.services.syncthing.group} ${config.services.syncthing.configDir}/cert.pem
          chmod 400 ${config.services.syncthing.configDir}/cert.pem
        ''}
        ${optionalString (cfg.key != null) ''
          cp ${toString cfg.key} ${config.services.syncthing.configDir}/key.pem
          chown ${config.services.syncthing.user}:${config.services.syncthing.group} ${config.services.syncthing.configDir}/key.pem
          chmod 400 ${config.services.syncthing.configDir}/key.pem
        ''}
      '';
    };

    systemd.services.syncthing-init = {
      after = [ "syncthing.service" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        User = config.services.syncthing.user;
        RemainAfterExit = true;
        Type = "oneshot";
        ExecStart = updateConfig;
      };
    };
  };
}

{ config, pkgs, ... }: with import <stockholm/lib>;

let

  kcfg = config.krebs.syncthing;
  scfg = config.services.syncthing;

  devices = mapAttrsToList (name: peer: {
    name = name;
    deviceID = peer.id;
    addresses = peer.addresses;
  }) kcfg.peers;

  folders = mapAttrsToList ( _: folder: {
    inherit (folder) path id type;
    devices = map (peer: { deviceId = kcfg.peers.${peer}.id; }) folder.peers;
    rescanIntervalS = folder.rescanInterval;
    fsWatcherEnabled = folder.watch;
    fsWatcherDelayS = folder.watchDelay;
    ignoreDelete = folder.ignoreDelete;
    ignorePerms = folder.ignorePerms;
  }) kcfg.folders;

  getApiKey = pkgs.writeDash "getAPIKey" ''
    ${pkgs.libxml2}/bin/xmllint \
      --xpath 'string(configuration/gui/apikey)'\
      ${scfg.dataDir}/config.xml
  '';

  updateConfig = pkgs.writeDash "merge-syncthing-config" ''
    set -efu

    # XXX this assumes the GUI address to be "IPv4 address and port"
    host=${shell.escape (elemAt (splitString ":" scfg.guiAddress) 0)}
    port=${shell.escape (elemAt (splitString ":" scfg.guiAddress) 1)}

    # wait for service to restart
    ${pkgs.untilport}/bin/untilport "$host" "$port"

    API_KEY=$(${getApiKey})

    _curl() {
      ${pkgs.curl}/bin/curl \
          -Ss \
          -H "X-API-Key: $API_KEY" \
          "http://$host:$port/rest""$@"
    }

    old_config=$(_curl /system/config)
    patch=${shell.escape (toJSON {
      inherit devices folders;
    })}
    new_config=$(${pkgs.jq}/bin/jq -en \
        --argjson old_config "$old_config" \
        --argjson patch "$patch" \
        '
          $old_config * $patch
        '
    )
    echo $new_config | _curl /system/config -d @-
    _curl /system/restart -X POST
  '';

in

{
  options.krebs.syncthing = {

    enable = mkEnableOption "syncthing-init";

    id = mkOption {
      type = types.str;
      default = config.krebs.build.host.name;
    };

    cert = mkOption {
      type = types.nullOr types.absolute-pathname;
      default = null;
    };

    key = mkOption {
      type = types.nullOr types.absolute-pathname;
      default = null;
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

          ignoreDelete = mkOption {
            type = types.bool;
            default = false;
          };

          ignorePerms = mkOption {
            type = types.bool;
            default = true;
          };

        };
      }));
    };
  };

  config = mkIf kcfg.enable {

    systemd.services.syncthing = mkIf (kcfg.cert != null || kcfg.key != null) {
      preStart = ''
        ${optionalString (kcfg.cert != null) ''
          cp ${toString kcfg.cert} ${scfg.dataDir}/cert.pem
          chown ${scfg.user}:${scfg.group} ${scfg.dataDir}/cert.pem
          chmod 400 ${scfg.dataDir}/cert.pem
        ''}
        ${optionalString (kcfg.key != null) ''
          cp ${toString kcfg.key} ${scfg.dataDir}/key.pem
          chown ${scfg.user}:${scfg.group} ${scfg.dataDir}/key.pem
          chmod 400 ${scfg.dataDir}/key.pem
        ''}
      '';
    };

    systemd.services.syncthing-init = {
      after = [ "syncthing.service" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        User = scfg.user;
        RemainAfterExit = true;
        Type = "oneshot";
        ExecStart = updateConfig;
      };
    };
  };
}

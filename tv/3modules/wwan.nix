with import ./lib;
{ config, pkgs, ... }: {
  options = {
    tv.wwan.enable = mkEnableOption "tv.wwan";
    tv.wwan.apn = mkOption {
      type = with types; filename;
    };
    tv.wwan.device = mkOption {
      type = with types; pathname;
      default = "/dev/cdc-wdm0";
    };
    tv.wwan.interface = mkOption {
      type = with types; nullOr filename;
      default = null;
    };
    tv.wwan.operators = mkOption {
      type = with types; listOf username;
      default = [];
    };
    tv.wwan.secrets = mkOption {
      type = with types; pathname;
      default = toString <secrets/wwan.json>;
      # format: {"pin1":number}
    };
  };
  config = let
    cfg = config.tv.wwan;
  in mkIf cfg.enable {
    nixpkgs.overlays = singleton (self: super: {
      uqmi-wrapper = pkgs.symlinkJoin {
        name = "uqmi-wrapper";
        paths = [
          (pkgs.writeDashBin "uqmi" ''
            exec ${pkgs.uqmi}/bin/uqmi --device=${cfg.device} "$@"
          '')
          pkgs.uqmi
        ];
      };
    });
    systemd.services.wwan = {
      environment = {
        SECRETS = "%d/secrets";
      };
      path = [
        pkgs.busybox
        pkgs.coreutils
        pkgs.iproute2
        pkgs.jq
        pkgs.uqmi-wrapper
        (pkgs.writeDashBin "get-interface" (
          if cfg.interface != null then /* sh */ ''
            echo ${cfg.interface}
          '' else /* sh */ ''
            exec ${pkgs.libqmi}/bin/qmicli -d ${cfg.device} -p --get-wwan-iface
          ''
        ))
      ];
      serviceConfig = {
        LoadCredential = [
          "secrets:${cfg.secrets}"
        ];
        Type = "oneshot";
        RemainAfterExit = true;
        SyslogIdentifier = "wwan";
        ExecStart = pkgs.writeDash "tv.wwan.start.sh" ''
          set -efu

          interface=$(get-interface)

          pin1_status=$(
            uqmi --uim-get-sim-state |
            jq -r '"\(.pin1_status)/\(.pin1_verify_tries)"'
          )
          case $pin1_status in
            verified/*)
              :
              ;;
            not_verified/3)
              pin1=$(jq .pin1 "$SECRETS")
              echo "verifying PIN1" >&2
              if ! uqmi --uim-verify-pin1 "$pin1"; then
                echo "error: failed to verify PIN1" >&2
                exit 1
              fi
              ;;
            not_verified/*)
              echo "error: not trying to verify PIN1: not enough tries left" >&2
              echo \
                  "please check your configuration in ${cfg.secrets}" \
                  " and verify if manually using:" \
                  " ${pkgs.uqmi}/bin/uqmi -d $device --uim-veriy-pin1 XXXX" \
                  >&2
              exit 1
          esac

          raw_ip_path=/sys/class/net/$interface/qmi/raw_ip
          raw_ip=$(cat "$raw_ip_path")
          if [ "$raw_ip" != Y ]; then
            echo "enabling raw-ip" >&2
            if ! echo Y > "$raw_ip_path"; then
              echo "error: failed to enable raw-ip" >&2
              exit 1
            fi
          fi

          operating_mode=$(uqmi --get-device-operating-mode | tr -d \")
          case $operating_mode in
            online)
              :
              ;;
            persistent_low_power|low_power)
              echo "settings device operating mode to online" >&2
              uqmi --set-device-operating-mode online
              operating_mode=$(uqmi --get-device-operating-mode | tr -d \")
              if test "$operating_mode" != online; then
                echo "error: failed to set device operating mode to online" >&2
                exit 1
              fi
              ;;
            *)
              echo "error: don't know how to change device operating mode to online: $operating_mode" >&2
              exit 1
          esac

          ip link set dev "$interface" up

          data_status=$(uqmi --get-data-status | tr -d \")
          case $data_status in
            connected)
              :
              ;;
            disconnected)
              echo "starting network (APN=${cfg.apn})" >&2
              sleep 1
              uqmi \
                  --start-network \
                  --autoconnect \
                  --apn ${cfg.apn} \
                  --ip-family ipv4
              sleep 1
              ;;
            *)
              echo "error: unsupported data status: $data_status" >&2
              exit 1
          esac

          udhcpc -q -f -n -i "$interface"
        '';
        Restart = "on-failure";
        ExecStop = pkgs.writeDash "tv.wwan.stop.sh" ''
          set -efu

          interface=$(get-interface)

          ip link set dev "$interface" down
          uqmi --stop-network 0xFFFFFFFF --autoconnect
          uqmi --sync
          uqmi --set-device-operating-mode persistent_low_power
        '';
      };
    };
    users.users.root.packages = [
      pkgs.uqmi-wrapper
    ];
    tv.systemd.services.wwan.operators = cfg.operators;
  };
}

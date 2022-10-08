{ config, lib, pkgs, ... }@args: with import <stockholm/lib>; let

  cfg = config.tv.ejabberd;

  gen-dhparam = pkgs.writeDash "gen-dhparam" ''
    set -efu
    path=$1
    bits=2048
    # TODO regenerate dhfile after some time?
    if ! test -e "$path"; then
      ${pkgs.openssl}/bin/openssl dhparam "$bits" > "$path"
    fi
  '';

  settingsFormat = pkgs.formats.yaml {};

in {
  options.tv.ejabberd = {
    enable = mkEnableOption "tv.ejabberd";
    certfiles = mkOption {
      type = types.listOf types.absolute-pathname;
      default = [
        (toString <secrets> + "/ejabberd.pem")
      ];
    };
    configFile = mkOption {
      type = types.either types.package types.absolute-pathname;
      default =
        (settingsFormat.generate "ejabberd.yaml" cfg.settings)
          # XXX ejabberd cannot parse MQTT topic filters enclosed in single
          # quotes.  By changing the YAML formatting style, double quotes will
          # be used instead.
          #
          # Related error message:
          #   Invalid value of option modules->mod_mqtt->access_publish:
          #   Malformed topic filter
          #
          .overrideAttrs (old: {
            nativeBuildInputs =
              filter
                (pkg: (parseDrvName pkg.name).name != "remarshal")
                old.nativeBuildInputs
              ++
              singleton (pkgs.symlinkJoin {
                name = "remarshal";
                paths = [
                  (pkgs.writeDashBin "json2yaml" ''
                    exec ${pkgs.remarshal}/bin/json2yaml --yaml-style \> "$@"
                  '')
                  pkgs.remarshal
                ];
              });
        });
    };
    ciphers = mkOption {
      type = types.listOf types.str;
      default = [
        "ECDHE-ECDSA-AES256-GCM-SHA384"
        "ECDHE-RSA-AES256-GCM-SHA384"
        "ECDHE-ECDSA-CHACHA20-POLY1305"
        "ECDHE-RSA-CHACHA20-POLY1305"
        "ECDHE-ECDSA-AES128-GCM-SHA256"
        "ECDHE-RSA-AES128-GCM-SHA256"
        "ECDHE-ECDSA-AES256-SHA384"
        "ECDHE-RSA-AES256-SHA384"
        "ECDHE-ECDSA-AES128-SHA256"
        "ECDHE-RSA-AES128-SHA256"
      ];
    };
    credentials.certfiles = mkOption {
      internal = true;
      readOnly = true;
      default =
        imap
          (i: const /* yaml */ "/tmp/credentials/certfile${toJSON i}")
          cfg.certfiles;
    };
    hosts = mkOption {
      type = with types; listOf str;
    };
    pkgs.ejabberd = mkOption {
      type = types.package;
      default = pkgs.symlinkJoin {
        name = "ejabberd-wrapper";
        paths = [
          (pkgs.writeDashBin "ejabberdctl" ''
            exec ${pkgs.ejabberd}/bin/ejabberdctl \
                --config /etc/ejabberd/ejabberd.yaml \
                --ctl-config /etc/ejabberd/ejabberdctl.cfg \
                --logs ${cfg.stateDir} \
                --spool ${cfg.stateDir} \
                "$@"
          '')
          pkgs.ejabberd
        ];
      };
    };
    protocol_options = mkOption {
      type = types.listOf types.str;
      default = [
        "no_sslv2"
        "no_sslv3"
        "no_tlsv1"
        "no_tlsv1_10"
      ];
    };
    registration_watchers = mkOption {
      type = types.listOf types.str;
      default = [
        config.krebs.users.tv.mail
      ];
    };
    settings = mkOption {
      type = settingsFormat.type;
      default = {};
    };
    stateDir = mkOption {
      type = types.absolute-pathname;
      default = "/var/lib/ejabberd";
      readOnly = true;
    };
  };
  config = lib.mkIf cfg.enable {

    environment.etc."ejabberd/ejabberd.yaml".source = cfg.configFile;
    environment.etc."ejabberd/ejabberdctl.cfg".source =
      builtins.toFile "ejabberdctl.cfg" /* sh */ ''
        ERL_OPTIONS='-setcookie ${cfg.stateDir}/.erlang.cookie'
      '';

    environment.systemPackages = [
      (pkgs.symlinkJoin {
        name = "ejabberd-sudo-wrapper";
        paths = [
          (pkgs.writeDashBin "ejabberdctl" ''
            exec ${pkgs.systemd}/bin/systemd-run \
                --unit=ejabberdctl \
                --property=StateDirectory=ejabberd \
                --property=User=ejabberd \
                --collect \
                --pipe \
                --quiet \
                ${cfg.pkgs.ejabberd}/bin/ejabberdctl "$@"
          '')
          cfg.pkgs.ejabberd
        ];
      })
    ];

    krebs.systemd.services.ejabberd = {};

    systemd.services.ejabberd = {
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      reloadTriggers = [
        config.environment.etc."ejabberd/ejabberd.yaml".source
        config.environment.etc."ejabberd/ejabberdctl.cfg".source
      ];
      serviceConfig = {
        ExecStartPre = [
          "${pkgs.coreutils}/bin/ln -s \${CREDENTIALS_DIRECTORY} /tmp/credentials"
          "${gen-dhparam} ${cfg.stateDir}/dhfile"
        ];
        ExecStart = "${cfg.pkgs.ejabberd}/bin/ejabberdctl foreground";
        ExecStop = [
          "${cfg.pkgs.ejabberd}/bin/ejabberdctl stop"
          "${cfg.pkgs.ejabberd}/bin/ejabberdctl stopped"
        ];
        ExecReload = "${cfg.pkgs.ejabberd}/bin/ejabberdctl reload_config";
        LoadCredential =
          zipListsWith
            (dst: src: "${baseNameOf dst}:${src}")
            cfg.credentials.certfiles
            cfg.certfiles;
        LimitNOFILE = 65536;
        PrivateDevices = true;
        PrivateTmp = true;
        SyslogIdentifier = "ejabberd";
        StateDirectory = "ejabberd";
        User = "ejabberd";
        DynamicUser = true;
        TimeoutSec = 60;
        RestartSec = 5;
        Restart = "on-failure";
        Type = "notify";
        NotifyAccess = "all";
        WatchdogSec = 30;
      };
    };

    # preset config values
    tv.ejabberd.settings = {
      access_rules = {
        announce = mkDefault [{ allow = "admin"; }];
        local = mkDefault [{ allow = "local"; }];
        configure = mkDefault [{ allow = "admin"; }];
        register = mkDefault ["allow"];
        s2s = mkDefault ["allow"];
        trusted_network = mkDefault [{ allow = "loopback"; }];
      };

      acl = {
        local.user_regexp = mkDefault "";
        loopback.ip = mkDefault [
          "127.0.0.0/8"
          "::1/128"
          "::FFFF:127.0.0.1/128"
        ];
      };

      certfiles = mkDefault cfg.credentials.certfiles;

      hosts = mkDefault cfg.hosts;

      language = mkDefault "en";

      listen = mkDefault [
        {
          port = 5222;
          ip = "::";
          module = "ejabberd_c2s";
          shaper = "c2s_shaper";
          ciphers = concatStringsSep ":" cfg.ciphers;
          protocol_options = cfg.protocol_options;
          starttls = true;
          starttls_required = true;
          tls = false;
          tls_compression = false;
          max_stanza_size = 65536;
        }
        {
          port = 5269;
          ip = "::";
          module = "ejabberd_s2s_in";
          shaper = "s2s_shaper";
          dhfile = "${cfg.stateDir}/dhfile";
          max_stanza_size = 131072;
        }
      ];

      loglevel = mkDefault "4";

      modules = {
        mod_adhoc = mkDefault {};
        mod_admin_extra = mkDefault {};
        mod_announce.access = mkDefault "announce";
        mod_caps = mkDefault {};
        mod_carboncopy = mkDefault {};
        mod_client_state = mkDefault {};
        mod_configure = mkDefault {};
        mod_disco = mkDefault {};
        mod_echo = mkDefault {};
        mod_bosh = mkDefault {};
        mod_last = mkDefault {};
        mod_offline.access_max_user_messages = mkDefault "max_user_offline_messages";
        mod_ping = mkDefault {};
        mod_privacy = mkDefault {};
        mod_private = mkDefault {};
        mod_register = {
          access_from = mkDefault "deny";
          access = mkDefault "register";
          ip_access = mkDefault "trusted_network";
          registration_watchers = mkDefault cfg.registration_watchers;
        };
        mod_roster = mkDefault {};
        mod_shared_roster = mkDefault {};
        mod_stats = mkDefault {};
        mod_time = mkDefault {};
        mod_vcard.search = mkDefault false;
        mod_version = mkDefault {};
        mod_http_api = mkDefault {};
      };

      s2s_access = mkDefault "s2s";
      s2s_ciphers = concatStringsSep ":" cfg.ciphers;
      s2s_dhfile = mkDefault "${cfg.stateDir}/dhfile";
      s2s_protocol_options = mkDefault cfg.protocol_options;
      s2s_tls_compression = mkDefault false;
      s2s_use_starttls = mkDefault "required";

      shaper_rules = {
        max_user_offline_messages = mkDefault [
          { "5000" = "admin"; }
          100
        ];
        max_user_sessions = mkDefault 10;
        c2s_shaper = mkDefault [
          { "none" = "admin"; }
          "normal"
        ];
        s2s_shaper = mkDefault "fast";
      };
    };
  };
}

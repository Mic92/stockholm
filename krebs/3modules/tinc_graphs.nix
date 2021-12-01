{ config, lib, pkgs, ... }:

with import <stockholm/lib>;
let
  cfg = config.krebs.tinc_graphs;
  internal_dir = "${cfg.workingDir}/internal";
  external_dir = "${cfg.workingDir}/external";

  out = {
    options.krebs.tinc_graphs = api;
    config = lib.mkIf cfg.enable imp ;
  };

  api = {
    enable = mkEnableOption "tinc graphs";

    geodbPath = mkOption {
      type = types.str;
      description = "Path to geocitydb, defaults to geolite-legacy";
      default = "${pkgs.geolite-legacy}/share/GeoIP/GeoIPCity.dat";
    };

    hostsPath = mkOption {
      type = types.str;
      description = "Path to Hosts directory";
      default = "${config.krebs.tinc.retiolum.hostsPackage}";
      defaultText = "\${config.krebs.tinc.retiolum.hostsPackage}";
    };

    network = mkOption {
      type = types.str;
      description = "Tinc Network to use";
      default = "retiolum";
    };

    nginx = {
      enable = mkEnableOption "enable tinc_graphs to be served with nginx";

      anonymous = mkOption {
        type = types.attrsOf types.unspecified;
        default = {};
        description = ''
          nginx virtualHost options to be merged into the anonymous graphs
          vhost entry.
        '';
      };
      anonymous-domain = mkOption {
        type = types.str;
        description = ''
          external domainname to be used for anonymous graphs
          it will be used if you want to enable ACME
        '';
        default = "graph.krebsco.de";
      };

      complete = mkOption {
        type = types.attrsOf types.unspecified;
        description = ''
          nginx virtualHost options to be merged into the complete graphs
          vhost entry.
        '';
      };
    };

    workingDir = mkOption {
      type = types.str;
      description = ''
        Path to working dir, will create interal and external/.
        Defaults to the new users home dir which defaults to
        /var/cache/tinc_graphs'';
      default = config.users.extraUsers.tinc_graphs.home;
      defaultText = "<literal>\${config.users.extraUsers.tinc_graphs.home}</literal>";
    };

    timerConfig = mkOption {
      type = with types; attrsOf str;
      default = {
        OnCalendar = "*:0/15";
      };
    };
  };

  imp = {
    environment.systemPackages = [ pkgs.tinc_graphs ];
    systemd.timers.tinc_graphs = {
      description = "Build Tinc Graphs via via timer";
      wantedBy = [ "timers.target" ];
      timerConfig = cfg.timerConfig;
    };
    systemd.services.tinc_graphs = {
      description = "Build Tinc Graphs";
      environment = {
        EXTERNAL_FOLDER = external_dir;
        INTERNAL_FOLDER = internal_dir;
        GEODB = cfg.geodbPath;
        TINC_HOSTPATH = cfg.hostsPath;
        TINC_NETWORK = cfg.network;
      };

      restartIfChanged = true;
      serviceConfig = {
        Type = "simple";
        TimeoutSec = 300; # we will wait 5 minutes, kill otherwise
        restart = "always";

        ExecStartPre = pkgs.writeDash "tinc_graphs-init" ''
          mkdir -p "${internal_dir}" "${external_dir}"
          if ! test -e "${cfg.workingDir}/internal/index.html"; then
            cp -fr "$(${pkgs.tinc_graphs}/bin/tincstats-static-dir)/internal/." "${internal_dir}"
          fi
          if ! test -e "${cfg.workingDir}/external/index.html"; then
            cp -fr "$(${pkgs.tinc_graphs}/bin/tincstats-static-dir)/external/." "${external_dir}"
          fi
        '';
        ExecStart = ''${pkgs.tinc_graphs}/bin/all-the-graphs "${cfg.network}"'';

        ExecStartPost = pkgs.writeDash "tinc_graphs-post" ''
          # TODO: this may break if workingDir is set to something stupid
          # this is needed because homedir is created with 700
          chmod 755  "${cfg.workingDir}"
        '';
        PrivateTmp = "yes";

        User = "root"; # tinc cannot be queried as user,
                       #  seems to be a tinc-pre issue
      };
    };

    users.extraUsers.tinc_graphs = {
      uid = genid_uint31 "tinc_graphs";
      group = "tinc_graphs";
      home = "/var/spool/tinc_graphs";
      isSystemUser = true;
    };
    users.groups.tinc_graphs = {};

    services.nginx = mkIf cfg.nginx.enable {
      enable = mkDefault true;
      virtualHosts = {
        tinc_graphs_complete = mkMerge [ cfg.nginx.complete  {
          locations = {
            "/".extraConfig = "autoindex on;";
            "/".root = internal_dir;
          };
        }];
        "${cfg.nginx.anonymous-domain}" = mkMerge [ cfg.nginx.anonymous {
          locations = {
            "/".extraConfig = "autoindex on;";
            "/".root = external_dir;
          };
        }];
      };
    };
  };

in
out

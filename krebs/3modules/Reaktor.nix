{ config, pkgs,lib, ... }:


let
  kpkgs = import ../5pkgs { inherit pkgs; inherit lib; };

  inherit (lib)
    mkIf
    mkOption
    types
    singleton
    isString
    optionalString
    concatStrings
    escapeShellArg
  ;

  ReaktorConfig = pkgs.writeText "config.py" ''
      ${if (isString cfg.overrideConfig ) then ''
      # Overriden Config
      ${cfg.overrideConfig}
      '' else ""}
      ## Extra Config
      ${cfg.extraConfig}
    '';
  cfg = config.krebs.Reaktor;

  out = {
    options.krebs.Reaktor = api;
    config = mkIf cfg.enable imp;
  };

  api = {
    enable = mkOption {
      default = false;
      description = ''
        Start Reaktor at system boot
      '';
    };

    nickname = mkOption {
      default = config.krebs.build.host.name + "|r";
      type = types.string;
      description = ''
        The nick name of the irc bot.
        Defaults to {hostname}|r
      '';
    };


    overrideConfig = mkOption {
      default = null;
      type = types.nullOr types.str;
      description = ''
        configuration to be used instead of default ones.
        Reaktor default cfg can be retrieved via `reaktor get-config`
      '';
    };
    extraConfig = mkOption {
      default = "";
      type = types.string;
      description = ''
        configuration appended to the default or overridden configuration
      '';
    };

    ReaktorPkg = mkOption {
      default = kpkgs.Reaktor;
      description = ''
        the Reaktor pkg to use.
      '';
    };
    debug = mkOption {
      default = false;
      description = ''
        Reaktor debug output
      '';
    };
  };

  imp = {
    # for reaktor get-config
    environment.systemPackages = [ cfg.ReaktorPkg ];
    users.extraUsers = singleton {
      name = "Reaktor";
      # uid = config.ids.uids.Reaktor;
      uid = 2066439104; #genid Reaktor
      description = "Reaktor user";
      home = "/var/lib/Reaktor";
      createHome = true;
    };

    #users.extraGroups = singleton {
    #  name = "Reaktor";
    #  gid = config.ids.gids.Reaktor;
    #};

    systemd.services.Reaktor = {
      path = with pkgs; [
        utillinux #flock for tell_on-join
        # git # for nag
        python # for caps
        ];
      description = "Reaktor IRC Bot";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      environment = {
        GIT_SSL_CAINFO = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
        REAKTOR_NICKNAME = cfg.nickname;
        REAKTOR_DEBUG = (if cfg.debug  then "True" else "False");
        };
      serviceConfig= {
        ExecStartPre = pkgs.writeScript "Reaktor-init" ''
          #! /bin/sh
          ${if (isString cfg.overrideConfig) then
            ''cp ${ReaktorConfig} /tmp/config.py''
          else
            ''(${cfg.ReaktorPkg}/bin/reaktor get-config;cat "${ReaktorConfig}" ) > /tmp/config.py''
          }
        '';
        ExecStart = "${cfg.ReaktorPkg}/bin/reaktor run /tmp/config.py";
        PrivateTmp = "true";
        User = "Reaktor";
        Restart = "on-abort";
        StartLimitInterval = "1m";
        StartLimitBurst = "1";
        };
    };
  };

in
out

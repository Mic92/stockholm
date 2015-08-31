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
  ;

  ReaktorConfig = mkIf ( isString cfg.extraConfig )  pkgs.writeText "config.py" cfg.extraConfig;
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
      type = types.str;
      description = ''
        The nick name of the irc bot.
        Defaults to {hostname}|r
      '';
    };


    extraConfig = mkOption {
      default = null;
      type = types.nullOr types.str;
      description = ''
        configuration to be used instead of default ones.
        Reaktor default cfg can be retrieved via `reaktor get-config`
      '';
    };

    ReaktorPkg = mkOption {
      default = kpkgs.Reaktor;
      description = ''
        the Reaktor pkg to use.
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
      serviceConfig.User = "Reaktor";
      environment = {
        GIT_SSL_CAINFO = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
        IRC_NICKNAME = cfg.nickname;
        };
      serviceConfig.ExecStart = "${cfg.ReaktorPkg}/bin/reaktor run ${if (isString cfg.extraConfig) then cfg.ReaktorConfig else ""}";
    };
  };

in
out

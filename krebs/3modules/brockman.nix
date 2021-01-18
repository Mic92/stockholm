{ pkgs, lib, config, ... }:
with lib;
let
  cfg = config.krebs.brockman;
in {
  options.krebs.brockman = {
    enable = mkEnableOption "brockman";
    config = mkOption { type = types.attrs; }; # TODO make real config here
  };

  config = mkIf cfg.enable {
    users.extraUsers.brockman = {
      home = "/var/lib/brockman";
      createHome = true;
      isNormalUser = false;
    };

    systemd.services.brockman = {
      description = "RSS to IRC broadcaster";
      wantedBy = [ "multi-user.target" ];
      after = [ "network-online.target" ];
      serviceConfig = {
        Restart = "always";
        ExecStart = ''
          ${pkgs.brockman}/bin/brockman ${pkgs.writeText "brockman.json" (builtins.toJSON cfg.config)}
        '';
        User = config.users.extraUsers.brockman.name;
        PrivateTmp = true;
        RuntimeDirectory = "brockman";
        WorkingDirectory = "%t/brockman";
      };
    };
  };
}

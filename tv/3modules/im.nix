{ config, pkgs, ... }: let
  im = config.tv.im;
  lib = import <stockholm/lib>;
in {
  options = {
    tv.im.client.enable = lib.mkEnableOption "tv.im.client" // {
      default = config.krebs.build.host.name == im.client.host.name;
    };
    tv.im.client.term = lib.mkOption {
      default = "rxvt-unicode-256color";
      type = lib.types.filename;
    };
    tv.im.client.useIPv6 = lib.mkEnableOption "tv.im.client.useIPv6" // {
      default = true;
    };
    tv.im.client.host = lib.mkOption {
      default = config.krebs.hosts.xu;
      type = lib.types.host;
    };
    tv.im.client.user = lib.mkOption {
      default = config.krebs.users.tv;
      type = lib.types.user;
    };

    tv.im.server.enable = lib.mkEnableOption "tv.im.server" // {
      default = config.krebs.build.host.name == im.server.host.name;
    };
    tv.im.server.host = lib.mkOption {
      default = config.krebs.hosts.nomic;
      type = lib.types.host;
    };
    tv.im.server.user = lib.mkOption {
      default = config.krebs.users.tv;
      type = lib.types.user;
    };
  };
  imports = [
    (lib.mkIf im.client.enable {
      users.users.${im.client.user.name}.packages = [
        (pkgs.writeDashBin "im" ''
          exec ${pkgs.openssh}/bin/ssh \
              ${lib.optionalString im.client.useIPv6 "-6"} \
              ${im.server.user.name}@${lib.head im.server.host.nets.retiolum.aliases} \
              -t \
              im
        '')
      ];
    })
    (lib.mkIf im.server.enable {
      services.bitlbee = {
        enable = true;
        plugins = [
          pkgs.bitlbee-facebook
        ];
      };
      users.users.${im.server.user.name}.packages = [
        (pkgs.writeDashBin "im" ''
          export PATH=${lib.makeSearchPath "bin" [
            pkgs.tmux
            pkgs.gnugrep
            pkgs.weechat
          ]}
          if tmux list-sessions -F\#S | grep -q '^im''$'; then
            exec tmux attach -t im
          else
            exec tmux new -s im weechat
          fi
        '')
      ];
    })
  ];
}

with import ./lib;
{ config, pkgs, ... }: let
  im = config.tv.im;
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
    tv.im.server.mosh.enable = lib.mkEnableOption "tv.im.server.mosh" // {
      default = true;
    };
    tv.im.server.weechat.relay.enable =
      lib.mkEnableOption "tv.im.server.weechat.relay";
    tv.im.server.user = lib.mkOption {
      default = config.krebs.users.tv;
      type = lib.types.user;
    };
  };
  imports = [
    (lib.mkIf im.client.enable {
      users.users.${im.client.user.name}.packages = [
        (pkgs.writeDashBin "im" ''
          ${if im.server.mosh.enable then /* sh */ ''
            exec ${pkgs.mosh}/bin/mosh \
                ${lib.optionalString im.client.useIPv6 "-6"} \
                ${im.server.user.name}@${lib.head im.server.host.nets.retiolum.aliases} \
                env TERM=${im.client.term} im
          '' else /* sh */ ''
            exec ${pkgs.openssh}/bin/ssh \
                ${lib.optionalString im.client.useIPv6 "-6"} \
                ${im.server.user.name}@${lib.head im.server.host.nets.retiolum.aliases} \
                -t \
                im
          ''}
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
        pkgs.mosh
        (pkgs.writeDashBin "im" ''
          export PATH=${lib.makeSearchPath "bin" [
            pkgs.tmux
            pkgs.gnugrep
            pkgs.weechat-tv
          ]}
          if tmux list-sessions -F\#S | grep -q '^im''$'; then
            exec tmux attach -t im
          else
            exec tmux new -s im weechat
          fi
        '')
      ];
    })
    (lib.mkIf im.server.mosh.enable {
      krebs.setuid.utempter = {
        filename = "${pkgs.libutempter}/lib/utempter/utempter";
        owner = "nobody";
        group = "utmp";
        mode = "2111";
      };
      tv.iptables.extra4.filter.Retiolum = [
        "-s ${im.client.host.nets.retiolum.ip4.addr} -p udp --dport 60000:61000 -j ACCEPT"
      ];
      tv.iptables.extra6.filter.Retiolum = [
        "-s ${im.client.host.nets.retiolum.ip6.addr} -p udp --dport 60000:61000 -j ACCEPT"
      ];
    })
    (lib.mkIf im.server.weechat.relay.enable {
      krebs.iana-etc.services = {
        "9001".tcp.name = "weechat-ssl";
      };
      tv.iptables.extra4.filter.Retiolum = [
        "-s ${im.client.host.nets.retiolum.ip4.addr} -p tcp -m tcp --dport 9001 -j ACCEPT"
      ];
      tv.iptables.extra6.filter.Retiolum = [
        "-s ${im.client.host.nets.retiolum.ip6.addr} -p tcp -m tcp --dport 9001 -j ACCEPT"
      ];
    })
  ];
}

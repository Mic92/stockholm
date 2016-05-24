{ config, pkgs, lib, ... }:
with config.krebs.lib;
let
  cfg = config.krebs.retiolum;

  out = {
    options.krebs.retiolum = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "krebs.retiolum";

    host = mkOption {
      type = types.host;
      default = config.krebs.build.host;
    };

    netname = mkOption {
      type = types.enum (attrNames cfg.host.nets);
      default = "retiolum";
      description = ''
        The tinc network name.
        It is used to name the TUN device and to generate the default value for
        <literal>config.krebs.retiolum.hosts</literal>.
      '';
    };

    extraConfig = mkOption {
      type = types.str;
      default = "";
      description = ''
        Extra Configuration to be appended to tinc.conf
      '';
    };

    tincPackage = mkOption {
      type = types.package;
      default = pkgs.tinc;
      description = "Tincd package to use.";
    };

    hosts = mkOption {
      type = with types; attrsOf host;
      default =
        filterAttrs (_: h: hasAttr cfg.netname h.nets) config.krebs.hosts;
      description = ''
        Hosts to generate <literal>config.krebs.retiolum.hostsPackage</literal>.
        Note that these hosts must have a network named
        <literal>config.krebs.retiolum.netname</literal>.
      '';
    };

    hostsPackage = mkOption {
      type = types.package;
      default = pkgs.stdenv.mkDerivation {
        name = "${cfg.netname}-tinc-hosts";
        phases = [ "installPhase" ];
        installPhase = ''
          mkdir $out
          ${concatStrings (mapAttrsToList (_: host: ''
            echo ${shell.escape host.nets.${cfg.netname}.tinc.config} \
              > $out/${shell.escape host.name}
          '') cfg.hosts)}
        '';
      };
      description = ''
        Package of tinc host configuration files.  By default, a package will
        be generated from <literal>config.krebs.retiolum.hosts</literal>.  This
        option's main purpose is to expose the generated hosts package to other
        modules, like <literal>config.krebs.tinc_graphs</literal>.  But it can
        also be used to provide a custom hosts directory.
      '';
      example = literalExample ''
        (pkgs.stdenv.mkDerivation {
          name = "my-tinc-hosts";
          src = /home/tv/my-tinc-hosts;
          installPhase = "cp -R . $out";
        })
      '';
    };

    iproutePackage = mkOption {
      type = types.package;
      default = pkgs.iproute;
      description = "Iproute2 package to use.";
    };

    privkey = mkOption {
      type = types.secret-file;
      default = {
        path = "${cfg.user.home}/tinc.rsa_key.priv";
        owner = cfg.user;
        source-path = toString <secrets> + "/${cfg.netname}.rsa_key.priv";
      };
    };

    connectTo = mkOption {
      type = types.listOf types.str;
      default = [ "fastpoke" "cd" "prism" "gum" ];
      description = ''
        The list of hosts in the network which the client will try to connect
        to.  These hosts should have an 'Address' configured which points to a
        routeable IPv4 or IPv6 address.

        In stockholm this can be done by configuring:
          krebs.hosts.${connect-host}.nets.${netname?"retiolum"}.via.addrs4 =
            [ "${external-ip} ${external-port}" ]
      '';
    };

    user = mkOption {
      type = types.user;
      default = {
        name = cfg.netname;
        home = "/var/lib/${cfg.user.name}";
      };
    };
  };

  imp = {
    krebs.secret.files."${cfg.netname}.rsa_key.priv" = cfg.privkey;

    environment.systemPackages = [ tinc iproute ];

    systemd.services.${cfg.netname} = {
      description = "Tinc daemon for Retiolum";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      requires = [ "secret.service" ];
      path = [ tinc iproute ];
      serviceConfig = rec {
        Restart = "always";
        ExecStart = "${tinc}/sbin/tincd -c ${confDir} -d 0 -U ${cfg.user.name} -D --pidfile=/var/run/tinc.${SyslogIdentifier}.pid";
        SyslogIdentifier = cfg.netname;
      };
    };

    users.users.${cfg.user.name} = {
      inherit (cfg.user) home name uid;
      createHome = true;
    };
  };

  net = cfg.host.nets.${cfg.netname};

  tinc = cfg.tincPackage;

  iproute = cfg.iproutePackage;

  confDir = let
    namePathPair = name: path: { inherit name path; };
  in pkgs.linkFarm "${cfg.netname}-etc-tinc" (mapAttrsToList namePathPair {
    "hosts" = cfg.hostsPackage;
    "tinc.conf" = pkgs.writeText "${cfg.netname}-tinc.conf" ''
      Name = ${cfg.host.name}
      Interface = ${cfg.netname}
      ${concatStrings (map (c: "ConnectTo = ${c}\n") cfg.connectTo)}
      PrivateKeyFile = ${cfg.privkey.path}
      ${cfg.extraConfig}
    '';
    "tinc-up" = pkgs.writeScript "${cfg.netname}-tinc-up" ''
      ${iproute}/sbin/ip link set ${cfg.netname} up
      ${optionalString (net.ip4 != null) ''
        ${iproute}/sbin/ip -4 addr add ${net.ip4.addr} dev ${cfg.netname}
        ${iproute}/sbin/ip -4 route add ${net.ip4.prefix} dev ${cfg.netname}
      ''}
      ${optionalString (net.ip6 != null) ''
        ${iproute}/sbin/ip -6 addr add ${net.ip6.addr} dev ${cfg.netname}
        ${iproute}/sbin/ip -6 route add ${net.ip6.prefix} dev ${cfg.netname}
      ''}
    '';
  });

in out

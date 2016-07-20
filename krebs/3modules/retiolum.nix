{ config, pkgs, lib, ... }:
with config.krebs.lib;
let
  out = {
    options.krebs.tinc = api;
    config = imp;
  };

  api = mkOption {
    default = {};
    description = ''
      define a tinc network
    '';
    type = with types; attrsOf (submodule (tinc: {
      options = {
        host = mkOption {
          type = types.host;
          default = config.krebs.build.host;
        };

        netname = mkOption {
          type = types.enum (attrNames tinc.config.host.nets);
          default = tinc.config._module.args.name;
          description = ''
            The tinc network name.
            It is used to name the TUN device and to generate the default value for
            <literal>config.krebs.tinc.retiolum.hosts</literal>.
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
            filterAttrs (_: h: hasAttr tinc.config.netname h.nets) config.krebs.hosts;
          description = ''
            Hosts to generate <literal>config.krebs.retiolum.hostsPackage</literal>.
            Note that these hosts must have a network named
            <literal>config.krebs.retiolum.netname</literal>.
          '';
        };

        hostsPackage = mkOption {
          type = types.package;
          default = pkgs.stdenv.mkDerivation {
            name = "${tinc.config.netname}-tinc-hosts";
            phases = [ "installPhase" ];
            installPhase = ''
              mkdir $out
              ${concatStrings (lib.mapAttrsToList (_: host: ''
                echo ${shell.escape host.nets."${tinc.config.netname}".tinc.config} \
                  > $out/${shell.escape host.name}
              '') tinc.config.hosts)}
            '';
          };
          description = ''
            Package of tinc host configuration files.  By default, a package will
            be generated from <literal>config.krebs.${tinc.config.netname}.hosts</literal>.  This
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
            path = "${tinc.config.user.home}/tinc.rsa_key.priv";
            owner = tinc.config.user;
            source-path = toString <secrets> + "/${tinc.config.netname}.rsa_key.priv";
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
            name = tinc.config.netname;
            home = "/var/lib/${tinc.config.user.name}";
          };
        };
      };
    }));
  };
  imp = lib.mkMerge ( lib.mapAttrsToList (netname: cfg:
  let
    net = cfg.host.nets.${netname};

    tinc = cfg.tincPackage;

    iproute = cfg.iproutePackage;

    confDir = let
      namePathPair = name: path: { inherit name path; };
    in pkgs.linkFarm "${netname}-etc-tinc" (lib.mapAttrsToList namePathPair {
      "hosts" = cfg.hostsPackage;
      "tinc.conf" = pkgs.writeText "${cfg.netname}-tinc.conf" ''
        Name = ${cfg.host.name}
        Interface = ${netname}
        ${concatStrings (map (c: "ConnectTo = ${c}\n") cfg.connectTo)}
        PrivateKeyFile = ${cfg.privkey.path}
        ${cfg.extraConfig}
      '';
      "tinc-up" = pkgs.writeDash "${netname}-tinc-up" ''
        ${iproute}/sbin/ip link set ${netname} up
        ${optionalString (net.ip4 != null) /* sh */ ''
          ${iproute}/sbin/ip -4 addr add ${net.ip4.addr} dev ${netname}
          ${iproute}/sbin/ip -4 route add ${net.ip4.prefix} dev ${netname}
        ''}
        ${optionalString (net.ip6 != null) /* sh */ ''
          ${iproute}/sbin/ip -6 addr add ${net.ip6.addr} dev ${netname}
          ${iproute}/sbin/ip -6 route add ${net.ip6.prefix} dev ${netname}
        ''}
      '';
    });
  in {
    krebs.secret.files."${netname}.rsa_key.priv" = cfg.privkey;

    environment.systemPackages = [ tinc iproute ];

    systemd.services.${netname} = {
      description = "Tinc daemon for ${netname}";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      requires = [ "secret.service" ];
      path = [ tinc iproute ];
      serviceConfig = rec {
        Restart = "always";
        ExecStart = "${tinc}/sbin/tincd -c ${confDir} -d 0 -U ${cfg.user.name} -D --pidfile=/var/run/tinc.${SyslogIdentifier}.pid";
        SyslogIdentifier = netname;
      };
    };

    users.users.${cfg.user.name} = {
      inherit (cfg.user) home name uid;
      createHome = true;
    };
  }) {} ); # TODO <<<< replace with the "config.krebs.tinc" and avoid infinite recursion
in out

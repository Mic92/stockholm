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

    name = mkOption {
      type = types.str;
      default = config.networking.hostName;
      # Description stolen from tinc.conf(5).
      description = ''
        This is the name which identifies this tinc daemon.  It must
        be unique for the virtual private network this daemon will
        connect to.  The Name may only consist of alphanumeric and
        underscore characters.  If Name starts with a $, then the
        contents of the environment variable that follows will be
        used.  In that case, invalid characters will be converted to
        underscores.  If Name is $HOST, but no such environment
        variable exist, the hostname will be read using the
        gethostnname() system call This is the name which identifies
        the this tinc daemon.
      '';
    };

    netname = mkOption {
      type = types.str;
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


    privateKeyFile = mkOption {
      # TODO if it's types.path then it gets copied to /nix/store with
      #      bad unsafe permissions...
      type = types.str;
      default = toString <secrets/retiolum.rsa_key.priv>;
      description = ''
          Generate file with <literal>tincd -K</literal>.
          This file must exist on the local system. The default points to 
          <secrets/retiolum.rsa_key.priv>.
        '';
    };

    connectTo = mkOption {
      type = types.listOf types.str;
      default = [ "fastpoke" "pigstarter" "gum" ];
      description = ''
        The list of hosts in the network which the client will try to connect
        to.  These hosts should have an 'Address' configured which points to a
        routeable IPv4 or IPv6 address.
      '';
    };

  };

  imp = {
    environment.systemPackages = [ tinc iproute ];

    systemd.services.retiolum = {
      description = "Tinc daemon for Retiolum";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      path = [ tinc iproute ];
      serviceConfig = rec {
        PermissionsStartOnly = "true";
        PrivateTmp = "true";
        Restart = "always";
        # TODO we cannot chroot (-R) b/c we use symlinks to hosts
        #      and the private key.
        ExecStartPre = pkgs.writeScript "retiolum-init" ''
          #! /bin/sh
          install -o ${user.name} -m 0400 ${cfg.privateKeyFile} /tmp/retiolum-rsa_key.priv
        '';
        ExecStart = "${tinc}/sbin/tincd -c ${confDir} -d 0 -U ${user.name} -D --pidfile=/var/run/tinc.${SyslogIdentifier}.pid";
        SyslogIdentifier = "retiolum";
      };
    };

    users.extraUsers = singleton {
      inherit (user) name uid;
    };
  };

  user = rec {
    name = "retiolum";
    uid = genid name;
  };

  tinc = cfg.tincPackage;

  iproute = cfg.iproutePackage;

  confDir = pkgs.runCommand "retiolum" {
    # TODO text
    executable = true;
    preferLocalBuild = true;
  } ''
    set -euf

    mkdir -p $out

    ln -s ${cfg.hostsPackage} $out/hosts

    cat > $out/tinc.conf <<EOF
    Name = ${cfg.name}
    Device = /dev/net/tun
    Interface = ${cfg.netname}
    ${concatStrings (map (c : "ConnectTo = " + c + "\n") cfg.connectTo)}
    PrivateKeyFile = /tmp/retiolum-rsa_key.priv
    ${cfg.extraConfig}
    EOF

    # source: krebscode/painload/retiolum/scripts/tinc_setup/tinc-up
    cat > $out/tinc-up <<EOF
    host=$out/hosts/${cfg.name}
    ${iproute}/sbin/ip link set \$INTERFACE up

    addr4=\$(sed -n 's|^ *Subnet *= *\(10[.][^ ]*\) *$|\1|p' \$host)
    if [ -n "\$addr4" ];then
        ${iproute}/sbin/ip -4 addr add \$addr4 dev \$INTERFACE
        ${iproute}/sbin/ip -4 route add 10.243.0.0/16 dev \$INTERFACE
    fi
    addr6=\$(sed -n 's|^ *Subnet *= *\(42[:][^ ]*\) *$|\1|p' \$host)
    ${iproute}/sbin/ip -6 addr add \$addr6 dev \$INTERFACE
    ${iproute}/sbin/ip -6 route add 42::/16 dev \$INTERFACE
    EOF

    chmod +x $out/tinc-up
  '';

in out

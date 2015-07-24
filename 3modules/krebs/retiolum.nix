{ config, pkgs, lib, ... }:

with builtins;
with lib;
let
  cfg = config.krebs.retiolum;

  out = {
    options.krebs.retiolum = api;
    config = mkIf cfg.enable imp;
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

    generateEtcHosts = mkOption {
      type = types.str;
      default = "both";
      description = ''
        If set to <literal>short</literal>, <literal>long</literal>, or <literal>both</literal>,
        then generate entries in <filename>/etc/hosts</filename> from subnets.
      '';
    };

    network = mkOption {
      type = types.str;
      default = "retiolum";
      description = ''
        The tinc network name.
        It is used to generate long host entries,
        and name the TUN device.
      '';
    };

    tincPackage = mkOption {
      type = types.package;
      default = pkgs.tinc;
      description = "Tincd package to use.";
    };

    hosts = mkOption {
      default = null;
      description = ''
        Hosts package or path to use.
        If a path is given, then it will be used to generate an ad-hoc package.
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
      default = "/root/src/secrets/retiolum.rsa_key.priv";
      description = "Generate file with <literal>tincd -K</literal>.";
    };

    connectTo = mkOption {
      type = types.listOf types.str;
      default = [ "fastpoke" "pigstarter" "kheurop" ];
      description = ''
        The list of hosts in the network which the client will try to connect
        to.  These hosts should have an 'Address' configured which points to a
        routeable IPv4 or IPv6 address.
      '';
    };

  };

  imp = {
    environment.systemPackages = [ tinc hosts iproute ];

    networking.extraHosts = retiolumExtraHosts;

    systemd.services.retiolum = {
      description = "Tinc daemon for Retiolum";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      path = [ tinc iproute ];
      serviceConfig = {
        PermissionsStartOnly = "true";
        PrivateTmp = "true";
        Restart = "always";
        # TODO we cannot chroot (-R) b/c we use symlinks to hosts
        #      and the private key.
        ExecStartPre = pkgs.writeScript "retiolum-init" ''
          #! /bin/sh
          install -o ${user.name} -m 0400 ${cfg.privateKeyFile} /tmp/retiolum-rsa_key.priv
        '';
        ExecStart = "${tinc}/sbin/tincd -c ${confDir} -d 0 -U ${user.name} -D";
        SyslogIdentifier = "retiolum";
      };
    };

    users.extraUsers = singleton {
      inherit (user) name uid;
    };
  };

  user = {
    name = "retiolum";
    uid = 301281149; # genid retiolum
  };

  tinc = cfg.tincPackage;
  hostsType = builtins.typeOf cfg.hosts;
  hosts =
    if hostsType == "package" then
      # use package as is
      cfg.hosts
    else if hostsType == "path" then
      # use path to generate a package
      pkgs.stdenv.mkDerivation {
        name = "custom-retiolum-hosts";
        src = cfg.hosts;
        installPhase = ''
          mkdir $out
          find . -name .git -prune -o -type f -print0 | xargs -0 cp --target-directory $out
        '';
      }
    else
      abort "The option `services.retiolum.hosts' must be set to a package or a path"
    ;
  iproute = cfg.iproutePackage;

  retiolumExtraHosts = import (pkgs.runCommand "retiolum-etc-hosts"
    { }
    ''
      generate() {
        (cd ${hosts}
          printf \'\'
          for i in `ls`; do
            names=$(hostnames $i)
            for j in `sed -En 's|^ *Aliases *= *(.+)|\1|p' $i`; do
              names="$names $(hostnames $j)"
            done
            sed -En '
              s|^ *Subnet *= *([^ /]*)(/[0-9]*)? *$|\1  '"$names"'|p
            ' $i
          done | sort
          printf \'\'
        )
      }

      case ${cfg.generateEtcHosts} in
        short)
          hostnames() { echo "$1"; }
          generate
          ;;
        long)
          hostnames() { echo "$1.${cfg.network}"; }
          generate
          ;;
        both)
          hostnames() { echo "$1.${cfg.network} $1"; }
          generate
          ;;
        *)
          echo '""'
          ;;
      esac > $out
    '');


  confDir = pkgs.runCommand "retiolum" {
    # TODO text
    executable = true;
    preferLocalBuild = true;
  } ''
    set -euf

    mkdir -p $out

    ln -s ${hosts} $out/hosts

    cat > $out/tinc.conf <<EOF
    Name = ${cfg.name}
    Device = /dev/net/tun
    Interface = ${cfg.network}
    ${concatStrings (map (c : "ConnectTo = " + c + "\n") cfg.connectTo)}
    PrivateKeyFile = /tmp/retiolum-rsa_key.priv
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
in
out

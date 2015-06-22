{ cfg, config, lib, pkgs, ... }:

let
  inherit (lib) concatStrings singleton;

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
    PrivateKeyFile = ${cfg.privateKeyFile}
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


  user = cfg.network + "-tinc";

in

{
  environment.systemPackages = [ tinc hosts iproute ];

  networking.extraHosts = retiolumExtraHosts;

  systemd.services.retiolum = {
    description = "Tinc daemon for Retiolum";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    path = [ tinc iproute ];
    serviceConfig = {
      # TODO we cannot chroot (-R) b/c we use symlinks to hosts
      #      and the private key.
      ExecStart = "${tinc}/sbin/tincd -c ${confDir} -d 0 -U ${user} -D";
      SyslogIdentifier = "retiolum-tincd";
    };
    restartIfChanged = true;
  };

  users.extraUsers = singleton {
    name = user;
    uid = 2961822815; # bin/genid retiolum-tinc
  };
}

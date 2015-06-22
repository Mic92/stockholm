{ config, lib, pkgs, ... }:

let
  inherit (lib) mkOption types;
in

{
  enable = mkOption {
    type = types.bool;
    default = false;
    description = "Enable tinc daemon for Retiolum.";
  };

  name = mkOption {
    type = types.string;
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
    type = types.string;
    default = "both";
    description = ''
      If set to <literal>short</literal>, <literal>long</literal>, or <literal>both</literal>,
      then generate entries in <filename>/etc/hosts</filename> from subnets.
    '';
  };

  network = mkOption {
    type = types.string;
    default = "retiolum";
    description = ''
      The tinc network name.
      It is used to generate long host entries,
      derive the name of the user account under which tincd runs,
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
    type = types.string;
    default = "/etc/tinc/retiolum/rsa_key.priv";
    description = "Generate file with <literal>tincd -K</literal>.";
  };

  connectTo = mkOption {
    type = types.listOf types.string;
    default = [ "fastpoke" "pigstarter" "kheurop" ];
    description = "TODO describe me";
  };

}

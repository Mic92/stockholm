{ config, pkgs, lib, ... }:
let
  slib = import ../../lib/pure.nix { inherit lib; };
  cfg = config.krebs.announce-activation;
  announce-activation = pkgs.writeDash "announce-activation" ''
    set -efu
    message=$(${cfg.get-message})
    exec ${pkgs.irc-announce}/bin/irc-announce \
        ${slib.shell.escape cfg.irc.server} \
        ${slib.shell.escape (toString cfg.irc.port)} \
        ${slib.shell.escape cfg.irc.nick} \
        ${slib.shell.escape cfg.irc.channel} \
        ${lib.escapeShellArg cfg.irc.tls} \
        "$message"
  '';
  default-get-message = pkgs.writeDash "announce-activation-get-message" ''
    set -efu
    PATH=${lib.makeBinPath [
      pkgs.coreutils
      pkgs.gawk
      pkgs.gnused
      pkgs.nix
    ]}
    profile=/nix/var/nix/profiles/system
    gen_info=$(nix-env -p "$profile" --list-generations | tail -1)
    gen_no=$(echo "$gen_info" | awk '{print$1}')
    pretty_name=$(sed -n '/^PRETTY_NAME=/{s/.*="//;s/"$//;p}' /etc/os-release)
    echo "activating generation $gen_no $pretty_name"
  '';
in {
  options.krebs.announce-activation = {
    enable = lib.mkEnableOption "announce-activation";
    get-message = lib.mkOption {
      default = default-get-message;
      type = lib.types.package;
    };
    irc = {
      # TODO rename channel to target?
      channel = lib.mkOption {
        default = "#xxx";
        type = lib.types.str; # TODO types.irc-channel
      };
      nick = lib.mkOption {
        default = config.krebs.build.host.name;
        type = slib.types.label;
      };
      port = lib.mkOption {
        default = 6667;
        type = lib.types.int;
      };
      server = lib.mkOption {
        default = "irc.r";
        type = slib.types.hostname;
      };
      tls = lib.mkOption {
        default = false;
        type = lib.types.bool;
      };
    };
  };
  config = lib.mkIf cfg.enable {
    system.activationScripts.announce-activation = lib.stringAfter [ "etc" ] ''
      ${announce-activation}
    '';
  };
}

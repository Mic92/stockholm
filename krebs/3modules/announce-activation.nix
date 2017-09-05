with import <stockholm/lib>;
{ config, pkgs, ... }: let
  cfg = config.krebs.announce-activation;
  announce-activation = pkgs.writeDash "announce-activation" ''
    set -efu
    message=$(${cfg.get-message})
    exec ${pkgs.irc-announce}/bin/irc-announce \
        ${shell.escape cfg.irc.server} \
        ${shell.escape (toString cfg.irc.port)} \
        ${shell.escape cfg.irc.nick} \
        ${shell.escape cfg.irc.channel} \
        "$message"
  '';
  default-get-message = pkgs.writeDash "announce-activation-get-message" ''
    set -efu
    PATH=${makeBinPath [
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
    enable = mkEnableOption "announce-activation";
    get-message = mkOption {
      default = default-get-message;
      type = types.package;
    };
    irc = {
      # TODO rename channel to target?
      channel = mkOption {
        default = "#retiolum";
        type = types.str; # TODO types.irc-channel
      };
      nick = mkOption {
        default = config.krebs.build.host.name;
        type = types.label;
      };
      port = mkOption {
        default = 6667;
        type = types.int;
      };
      server = mkOption {
        default = "ni.r";
        type = types.hostname;
      };
    };
  };
  config = mkIf cfg.enable {
    system.activationScripts.announce-activation = ''
      ${announce-activation}
    '';
  };
}

arg@{ config, pkgs, lib, ... }:

let
  inherit (lib) mkIf mkMerge;

  cfg = config.tv.git;
  arg' = arg // { inherit cfg; };
in

# TODO unify logging of shell scripts to user and journal
# TODO move all scripts to ${etcDir}, so ControlMaster connections
#       immediately pick up new authenticators
# TODO when authorized_keys changes, then restart ssh
#       (or kill already connected users somehow)

{
  imports = [
    ../../tv/nginx
  ];

  options.tv.git = import ./options.nix arg';

  config = mkIf cfg.enable (mkMerge [
    (import ./config.nix arg')
    (mkIf cfg.cgit (import ./cgit.nix arg'))
  ]);
}

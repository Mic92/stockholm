arg@{ config, lib, pkgs, ... }:

let
  cfg = config.tv.iptables;
  arg' = arg // { inherit cfg; };
in

{
  options.tv.iptables = import ./options.nix arg';
  config = lib.mkIf cfg.enable (import ./config.nix arg');
}

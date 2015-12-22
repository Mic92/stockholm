{ lib, config, pkgs, ... }:
let
    pkgs-unst = import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) {};
in {
  nixpkgs.config.packageOverrides = pkgs: {
    buildbot = pkgs-unst.buildbot;
    buildbot-slave = pkgs-unst.buildbot-slave;
  };
  networking.firewall.allowedTCPPorts = [ 8010 ];
  krebs.buildbot.master = {
    enable = true;
    irc = {
      enable = true;
      server = "cd.retiolum";
      channel = "retiolum";
      allowForce = true;
    };
    extraConfig = ''
      c['buildbotURL']  = "http://${config.krebs.build.host.name}:8010/"
    '';
  };

  krebs.buildbot.slave = {
    enable = true;
    masterhost = "localhost";
    username = "testslave";
    password = "krebspass";
    packages = with pkgs;[ git nix ];
    extraEnviron = { NIX_PATH="nixpkgs=${toString <nixpkgs>}"; };
  };
}

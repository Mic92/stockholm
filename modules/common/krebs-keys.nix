# alle public keys der krebsminister fuer R in krebs repos
{ config, ... }:

let
  inherit (builtins) readFile;
in

with import ../lass/sshkeys.nix {
  config.sshKeys.lass.pub = config.sshKeys.lass.pub;
  config.sshKeys.uriel.pub = config.sshKeys.uriel.pub;
  };
{
  imports = [
    ./sshkeys.nix
  ];

  config.sshKeys.tv.pub = readFile <pubkeys/tv_wu.ssh.pub>;
}

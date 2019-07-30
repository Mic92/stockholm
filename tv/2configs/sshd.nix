{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

{
  services.openssh = {
    enable = true;
  };
  tv.iptables.input-internet-accept-tcp = singleton "ssh";
}

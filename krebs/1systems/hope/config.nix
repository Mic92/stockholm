with import <stockholm/lib>;
{ config, pkgs, ... }: let

  ip = config.krebs.build.host.nets.internet.ip4.addr;
  bestGuessGateway = addr: elemAt (match "(.*)(\.[^.])" addr) 0 + ".1";

in {
  imports = [
    <stockholm/krebs>
    <stockholm/krebs/2configs>
    <stockholm/krebs/2configs/os-templates/CAC-CentOS-7-64bit.nix>

    <stockholm/krebs/2configs/secret-passwords.nix>
    {
      users.extraUsers = {
        satan = {
          name = "satan";
          uid = 1338;
          home = "/home/satan";
          group = "users";
          createHome = true;
          useDefaultShell = true;
          initialPassword = "test";
        };
      };
    }
  ];

  krebs.build.host = config.krebs.hosts.hope;

  networking = let
    address = config.krebs.build.host.nets.internet.ip4.addr;
  in {
    defaultGateway = bestGuessGateway address;
    interfaces.enp2s1.ip4 = singleton {
      inherit address;
      prefixLength = 24;
    };
    nameservers = ["8.8.8.8"];
  };
}

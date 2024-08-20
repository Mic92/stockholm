{ config, pkgs, ... }:
let
  shack-ip = config.krebs.build.host.nets.shack.ip4.addr;
in
{
  imports = [
    ./hardware-configuration.nix
    ../../../krebs
    ../../../krebs/2configs
    # ../../../krebs/2configs/secret-passwords.nix

    # ../../../krebs/2configs/binary-cache/nixos.nix
    # ../../../krebs/2configs/binary-cache/prism.nix
    ../../../krebs/2configs/shack/ssh-keys.nix
    ../../../krebs/2configs/shack/prometheus/node.nix
    # provides access to /home/share for smbuser via smb
    ../../../krebs/2configs/shack/share.nix
    {
      fileSystems."/home/share" =
        { device = "/serve";
          options = [ "bind" "nofail" ];
        };
    }

    ## Collect local statistics via collectd and send to collectd
    # ../../../krebs/2configs/stats/shack-client.nix
    # ../../../krebs/2configs/stats/shack-debugging.nix
  ];

  krebs.build.host = config.krebs.hosts.filebitch;

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="60:a4:4c:3d:52:cf", NAME="et0"
  '';
  networking = {
    firewall.enable = true;
    interfaces.et0.useDHCP = true;
    #interfaces.et0.ipv4.addresses = [
    #  {
    #    address = shack-ip;
    #    prefixLength = 20;
    #  }
    #];

    defaultGateway = "10.42.0.1";
    nameservers = [ "10.42.0.100" "10.42.0.200" ];
  };
}

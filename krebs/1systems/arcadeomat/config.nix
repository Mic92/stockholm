{ config,lib, pkgs, ... }:
let
  shack-ip = config.krebs.build.host.nets.shack.ip4.addr;
  ext-if = "et0";
  external-mac = "52:54:b0:0b:af:fe";
  mainUser = "krebs";

in
{
  imports = [
    ./hw.nix
    ../../../krebs
    ../../../krebs/2configs

    #../../../krebs/2configs/binary-cache/nixos.nix
    #../../../krebs/2configs/binary-cache/prism.nix

    ../../../krebs/2configs/shack/ssh-keys.nix
    ../../../krebs/2configs/save-diskspace.nix
    ../../../krebs/2configs/shack/prometheus/node.nix

  ];
  # use your own binary cache, fallback use cache.nixos.org (which is used by
  # apt-cacher-ng in first place)

  # local discovery in shackspace
  nixpkgs.config.packageOverrides = pkgs: { tinc = pkgs.tinc_pre; };
  krebs.tinc.retiolum.extraConfig = "TCPOnly = yes";


  #networking = {
  #  firewall.enable = false;
  #  firewall.allowedTCPPorts = [ 8088 8086 8083 ];
  #  interfaces."${ext-if}".ipv4.addresses = [
  #    {
  #      address = shack-ip;
  #      prefixLength = 20;
  #    }
  #  ];

  #  defaultGateway = "10.42.0.1";
  #  nameservers = [ "10.42.0.100" "10.42.0.200" ];
  #};

  #####################
  # uninteresting stuff
  #####################
  krebs.build.host = config.krebs.hosts.arcadeomat;
  users.users."${mainUser}" = {
    uid = 9001;
    extraGroups = [ "audio" "video" ];
    isNormalUser = true;
  };


  time.timeZone = "Europe/Berlin";

  # avahi
  services.avahi = {
    enable = true;
    wideArea = false;
  };
  environment.systemPackages = with pkgs;[ glxinfo sdlmame ];
  nixpkgs.config.allowUnfree = true;
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.legacy_340;
  boot.kernelPackages = pkgs.linuxPackages_5_4;

  services.xserver = {
    videoDrivers = [ "nvidia" ];
    enable = true;
    windowManager = {
      awesome.enable = true;
      awesome.noArgb = true;
      awesome.luaModules = [ pkgs.luaPackages.vicious ];
    };
    displayManager.defaultSession = lib.mkDefault "none+awesome";
    displayManager.autoLogin = {
      enable = true;
      user = mainUser;
    };
  };
}

with import ./lib;
{ config, pkgs, ... }: {
  imports = [
    ./disks.nix
    <stockholm/tv>
    <stockholm/tv/2configs/hw/x220.nix>
    <stockholm/tv/2configs/exim-retiolum.nix>
    <stockholm/tv/2configs/gitconfig.nix>
    <stockholm/tv/2configs/pulse.nix>
    <stockholm/tv/2configs/retiolum.nix>
    <stockholm/tv/2configs/xsessions>
  ];

  krebs.build.host = config.krebs.hosts.bu;

  networking.hostId = lib.mkDefault "00000000";

  networking.wireless.enable = true;
  networking.useDHCP = false;
  networking.interfaces.enp0s25.useDHCP = true;
  networking.interfaces.wlp3s0.useDHCP = true;
  networking.interfaces.wwp0s29u1u4i6.useDHCP = true;
  networking.wireless.interfaces = [
    "wlp3s0"
  ];

  programs.gnupg.agent.enable = true;
  programs.gnupg.agent.pinentryFlavor = "gtk2";

  services.earlyoom.enable = true;
  services.earlyoom.freeMemThreshold = 5;
  systemd.services.earlyoom.environment.EARLYOOM_ARGS = toString [
    "--prefer '(^|/)chromium$'"
  ];

  system.stateVersion = "21.11";
}

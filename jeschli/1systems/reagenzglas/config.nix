{ config, pkgs, ... }:
with pkgs;
let
  rebuild_script = pkgs.writeTextFile {
    name="rebuild";
    text=''
      #!/usr/bin/env sh
      set -eu
      sudo cp -r /etc/nixos ~/old-nixos
      sudo cp -r $HOME/nixos /etc/
      sudo nixos-rebuild switch 
      '';
    executable=true;
  };
in
{
  imports =
    [
    <stockholm/jeschli>
    <stockholm/jeschli/2configs/emacs.nix>
       ./desktop.nix
       ./i3-configuration.nix
       ./hardware-configuration.nix
    ];

  # EFI systemd boot loader
  boot.loader.systemd-boot.enable = true;

  # Wireless network with network manager
  krebs.build.host = config.krebs.hosts.brauerei;
  # networking.hostName = "nixos"; # Define your hostname.
  networking.networkmanager.enable = true;

  # Allow unfree
  nixpkgs.config.allowUnfree = true;

  # Select internationalisation properties.
  i18n = {
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget vim git
    firefox
    rofi
  ];

  # How I rebuild the system
  environment.shellAliases = {
    rebuild = rebuild_script;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.ombi = {
     isNormalUser = true;
     extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  users.users.jeschli = {
     isNormalUser = true;
  };

  services.xserver.synaptics.enable = true;

  #Enable ssh daemon
  services.openssh.enable = true;

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDM1xtX/SF2IzfAIzrXvH4HsW05eTBX8U8MYlEPadq0DS/nHC45hW2PSEUOVsH0UhBRAB+yClVLyN+JAYsuOoQacQqAVq9R7HAoFITdYTMJCxVs4urSRv0pWwTopRIh1rlI+Q0QfdMoeVtO2ZKG3KoRM+APDy2dsX8LTtWjXmh/ZCtpGl1O8TZtz2ZyXyv9OVDPnQiFwPU3Jqs2Z036c+kwxWlxYc55FRuqwRtQ48c/ilPMu+ZvQ22j1Ch8lNuliyAg1b8pZdOkMJF3R8b46IQ8FEqkr3L1YQygYw2M50B629FPgHgeGPMz3mVd+5lzP+okbhPJjMrUqZAUwbMGwGzZ ombi@nixos"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKFXgtbgeivxlMKkoEJ4ANhtR+LRMSPrsmL4U5grFUME jeschli@nixos"
  ];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?

}

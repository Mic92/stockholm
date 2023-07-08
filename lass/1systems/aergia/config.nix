{ config, lib, pkgs, ... }:

{
  imports = [
    <stockholm/lass>

    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/exim-retiolum.nix>
    <stockholm/lass/2configs/baseX.nix>
    <stockholm/lass/2configs/pipewire.nix>
    <stockholm/lass/2configs/browsers.nix>
    <stockholm/lass/2configs/programs.nix>
    <stockholm/lass/2configs/network-manager.nix>
    <stockholm/lass/2configs/syncthing.nix>
    <stockholm/lass/2configs/sync/sync.nix>
    <stockholm/lass/2configs/games.nix>
    <stockholm/lass/2configs/steam.nix>
    <stockholm/lass/2configs/wine.nix>
    <stockholm/lass/2configs/fetchWallpaper.nix>
    <stockholm/lass/2configs/yellow-mounts/samba.nix>
    <stockholm/lass/2configs/pass.nix>
    <stockholm/lass/2configs/mail.nix>
    <stockholm/lass/2configs/bitcoin.nix>
    # <stockholm/lass/2configs/xonsh.nix>
    <stockholm/lass/2configs/review.nix>
    <stockholm/lass/2configs/dunst.nix>
    <stockholm/lass/2configs/print.nix>
    <stockholm/lass/2configs/br.nix>
    <stockholm/lass/2configs/c-base.nix>
    # steam-deck like experience https://github.com/Jovian-Experiments/Jovian-NixOS
    {
      imports = [
        "${builtins.fetchTarball "https://github.com/Jovian-Experiments/Jovian-NixOS/archive/master.tar.gz"}/modules"
      ];
      jovian.steam.enable = true;
    }
  ];

  system.stateVersion = "22.11";

  krebs.build.host = config.krebs.hosts.aergia;

  environment.systemPackages = with pkgs; [
    brain
    bank
    l-gen-secrets
    generate-secrets
  ];

  programs.adb.enable = true;

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };
  hardware.pulseaudio.package = pkgs.pulseaudioFull;

  nix.trustedUsers = [ "root" "lass" ];

  # nix.extraOptions = ''
  #   extra-experimental-features = nix-command flakes
  # '';

  services.tor = {
    enable = true;
    client.enable = true;
  };

  documentation.nixos.enable = true;
  boot.binfmt.emulatedSystems = [
    "aarch64-linux"
  ];

  boot.cleanTmpDir = true;
}

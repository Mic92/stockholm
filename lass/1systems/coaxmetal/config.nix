{ config, lib, pkgs, ... }:

{
  imports = [
    <stockholm/lass>

    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/exim-retiolum.nix>
    <stockholm/lass/2configs/baseX.nix>
    <stockholm/lass/2configs/browsers.nix>
    <stockholm/lass/2configs/programs.nix>
    <stockholm/lass/2configs/network-manager.nix>
    <stockholm/lass/2configs/syncthing.nix>
    <stockholm/lass/2configs/sync/sync.nix>
    <stockholm/lass/2configs/games.nix>
    <stockholm/lass/2configs/steam.nix>
    <stockholm/lass/2configs/wine.nix>
    <stockholm/lass/2configs/fetchWallpaper.nix>
    <stockholm/lass/2configs/nfs-dl.nix>
    <stockholm/lass/2configs/pass.nix>
    <stockholm/lass/2configs/mail.nix>
    <stockholm/lass/2configs/bitcoin.nix>
  ];

  krebs.build.host = config.krebs.hosts.coaxmetal;

  environment.shellAliases = {
    deploy = pkgs.writeDash "deploy" ''
      set -eu
      export SYSTEM="$1"
      $(nix-build $HOME/sync/stockholm/lass/krops.nix --no-out-link --argstr name "$SYSTEM" -A deploy)
    '';
    usb-tether-on = pkgs.writeDash "usb-tether-on" ''
      adb shell su -c service call connectivity 33 i32 1 s16 text
    '';
    usb-tether-off = pkgs.writeDash "usb-tether-off" ''
      adb shell su -c service call connectivity 33 i32 0 s16 text
    '';
  };

  programs.adb.enable = true;

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
    # config.General.Disable = "Headset";
    extraConfig = ''
      [General]
      Disable = Headset
    '';
  };
  hardware.pulseaudio.package = pkgs.pulseaudioFull;
}

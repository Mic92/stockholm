{ pkgs, ... }:

let
  dummy_firmware = pkgs.writeTextFile {
    name = "brcmfmac4356-pcie.txt";
    text = builtins.readFile ./brcmfmac4356-pcie.txt;
    destination = "/lib/firmware/brcm/brcmfmac4356-pcie.txt";
  };
in {
  hardware.firmware = [ dummy_firmware ];

  boot.kernelPackages = pkgs.linuxPackages_4_14;
  boot.kernelParams = [
    "fbcon=rotate:1"
  ];
  services.tlp.enable = true;
  services.xserver.displayManager.sessionCommands = ''
    (sleep 2 && ${pkgs.xorg.xrandr}/bin/xrandr --output DSI1 --rotate right)
  '';
}

with import <stockholm/lib>;
{ config, pkgs, ... }: {

  environment.etc."utsushi.conf".text = ''
    [devices]
    dev1.udi = esci:networkscan://EPSON79678C.fritz.box:1865
    dev1.model = XP-332
    dev1.vendor = EPSON
  '';

  hardware.sane = {
    enable = true;
    extraBackends = [
      pkgs.utsushi-customized
    ];
  };

  krebs.nixpkgs.allowUnfreePredicate = pkg:
    packageName pkg == "imagescan-plugin-networkscan";

  nixpkgs.overlays = singleton (self: super: {
    utsushi-customized = self.utsushi.override {
      guiSupport = false;
      jpegSupport = false;
      networkSupport = true;
      ocrSupport = false;
      saneSupport = true;
      tiffSupport = true;

      logCategory = "ALL";
      logLevel = "BRIEF";
    };
  });

  services = {
    printing = {
      drivers = [
        pkgs.epson-escpr
      ];
      enable = true;
    };
    saned.enable = true;
  };

}

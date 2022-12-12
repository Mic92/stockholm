with import ./lib;
{ config, pkgs, ... }: {

  environment.etc."utsushi.conf".text = ''
    [devices]
    dev1.udi = esci:networkscan://ep.hkw:1865
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
    utsushi-customized = self.utsushi.overrideAttrs (old: {
      postInstall = ''
        ${old.postInstall or ""}
        ln -s /etc/utsushi.conf $out/etc/utsushi/utsushi.conf
        ln -s ${pkgs.imagescan-plugin-networkscan}/lib/utsushi/networkscan \
          $out/libexec/utsushi/
      '';
    });
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

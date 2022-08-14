with import <stockholm/lib>;
{ config, pkgs, ... }: {

  config = {
    environment.etc =
      mapAttrs'
        (name: pkg: {
          name = "zones/${name}";
          value.source = pkg;
        })
        pkgs.krebs.zones;

    nixpkgs.overlays = [
      # Explicit zones generated from config.krebs.hosts.*.extraZones
      (self: super: let
        stripEmptyLines = s: (concatStringsSep "\n"
          (remove "\n" (remove "" (splitString "\n" s)))) + "\n";
        all-zones = foldAttrs (sum: current: sum + "\n" + current) ""
          ([config.krebs.zone-head-config] ++ combined-hosts);
        combined-hosts =
          mapAttrsToList (name: getAttr "extraZones") config.krebs.hosts;
      in {
        krebs = super.krebs or {} // {
          zones = super.krebs.zones or {} //
            mapAttrs'
              (name: value: {
                name = name;
                value = self.writeText "${name}.zone" (stripEmptyLines value);
              })
              all-zones;
        };
      })
    ];
  };

}

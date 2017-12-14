with import <stockholm/lib>;
{ config, ... }: {

  config = {
    # Implements environment.etc."zones/<zone-name>"
    environment.etc = let
      stripEmptyLines = s: (concatStringsSep "\n"
        (remove "\n" (remove "" (splitString "\n" s)))) + "\n";
      all-zones = foldAttrs (sum: current: sum + "\n" +current ) ""
        ([config.krebs.zone-head-config] ++ combined-hosts);
      combined-hosts =
        mapAttrsToList (name: getAttr "extraZones") config.krebs.hosts;
    in
      mapAttrs'
        (name: value: {
          name = "zones/${name}";
          value.text = stripEmptyLines value;
        })
        all-zones;
  };

}

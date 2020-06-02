{ lib, ... }:
with import ../lib.nix { inherit lib; };

{
  lass.hass.config = lib.mkMerge [
    (detect_movement sensors.movement.essen lights.essen 10)
    (lightswitch switches.dimmer.essen lights.essen)
  ];
}

{ lib, ... }:
with import ../lib.nix { inherit lib; };

{
  lass.hass.config = lib.mkMerge [
    (lightswitch switches.dimmer.bett lights.bett)
  ];
}

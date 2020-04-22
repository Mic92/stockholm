{ lib, ... }:
with import ../lib.nix;

{
  lass.hass.config = lib.lists.fold lib.recursiveUpdate {} [
    {
      #automation = [{
      #  trigger = {
      #    platform = "mqtt";
      #    topic = "zigbee/0x00178801086ac38c/action";
      #    payload = "on-press";
      #  };
      #  action = {
      #    service = "light.turn_on";
      #    data = {
      #      brightness = 150;
      #      rgb_color = [ 255 0 0 ];
      #      entity_id = [
      #        "light.0x0017880108327622_light"
      #      ];
      #    };
      #  };
      #}];
    }
    (lightswitch sensors.bett lights.bett)
  ];
}

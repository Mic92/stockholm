{ lib, ... }:
with import ../lib.nix { inherit lib; };

{
  lass.hass.config = lib.mkMerge [
    (lightswitch switches.dimmer.bett lights.bett)
  ];

  lass.hass.love = {
    resources = [{
      url = "https://raw.githubusercontent.com/ljmerza/light-entity-card/master/dist/light-entity-card.js.map";
      type = "js";
    }];
    views = [{
      title = "bett";
      cards = [
        {
          type = "markdown";
          title = "hello world";
          content = "This is just a test";
        }
        {
          type = "light";
          entity = "light.${lights.bett}";
        }
        {
          type = "custom:light-entity-card";
          entity = "light.${lights.bett}";
        }
        {
          type = "history-graph";
          entities = [
            "light.${lights.bett}"
          ];
        }
      ];
    }];
  };
}

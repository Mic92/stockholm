# 1 - haupt
# 2 - dusche
# 3 - warmwasser
# 4 - or
# 5 - kueche
let
  nodelight = type: ident: name: {
    platform = "mqtt";
    name = "${type} ${name}";
    command_topic = "${type}/${toString ident}/command";
    state_topic = "${type}/${toString ident}/state";
    payload_on = "on";
    payload_off = "off";
  };
  power = nodelight "power";
  light = ident: name: { icon = "mdi:lightbulb";} // nodelight "light" ident name;
in
[
  (power 1 "Hauptschalter")
  (power 2 "Dusche")
  (power 3 "Warmwasser")
  (power 4 "Optionsräume")
  (power 5 "Küche")
  (light 1 "Decke Lounge 1")
  (light 2 "Decke Lounge 2")
  (light 3 "Decke Lounge 3")
  (light 4 "Decke Lounge 4")
  (light 5 "Decke Lounge 5")
  (light 6 "Decke Lounge 6")
  (light 7 "Decke Lounge 7")
  (light 8 "Decke Lounge 8")
]

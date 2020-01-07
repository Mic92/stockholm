let
  glados = import ../lib;
in
     (map glados.esphome.temp [ "lounge" "werkstatt" "herrenklo" "dusche" "fablab" "whc" ])
  ++ (map glados.esphome.hum  [ "lounge" "werkstatt" "herrenklo" "dusche" "fablab" "whc" ])

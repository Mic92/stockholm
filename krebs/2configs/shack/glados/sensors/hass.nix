let
  glados = import ../lib;
in
     (map (host: glados.esphome.temp {inherit host;})  [ "lounge" "werkstatt" "herrenklo" "dusche" "fablab" "whc" ])
  ++ (map (host: glados.esphome.hum  {inherit host;})  [ "lounge" "werkstatt" "herrenklo" "dusche" "fablab" "whc" ])

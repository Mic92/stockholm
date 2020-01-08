let
  glados = import ../lib;
in
     (map (name: glados.esphome.temp {inherit name;})  [ "lounge" "werkstatt" "herrenklo" "dusche" "fablab" "whc" ])
  ++ (map (name: glados.esphome.hum  {inherit name;})  [ "lounge" "werkstatt" "herrenklo" "dusche" "fablab" "whc" ])

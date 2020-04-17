let
  fuellstand =  name: id:  {
    platform = "rest";
    resource = "https://ora5.tutschonwieder.net/ords/lick_prod/v1/get/fuellstand/1/${toString id}";
    method = "GET";
    name = "Füllstand ${name}";
    value_template = "{{ value_json.fuellstand }}";
  };
in [
  (fuellstand "Wasser"           1)
  (fuellstand "Mate Cola"        2)
  (fuellstand "Apfelschorle"     3)
  (fuellstand "Zitronensprudel"  4)
  (fuellstand "Mate 1"           26)
  (fuellstand "Mate 2"           27)
]

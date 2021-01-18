{
  services.home-assistant.config.binary_sensor =
  [
    {
      platform = "rest";
      resource = "https://spaceapi.afra-berlin.de/v1/status.json";
      method = "GET";
      name = "Door AFRA Berlin";
      device_class = "door";
      value_template = "{{ value_json.open }}";
    }
    {
      platform = "rest";
      resource = "http://club.entropia.de/spaceapi";
      method = "GET";
      name = "Door Entropia";
      device_class = "door";
      value_template = "{{ value_json.open }}";
    }
    {
      platform = "rest";
      resource = "http://www.c-base.org/status.json";
      method = "GET";
      name = "Door C-Base Berlin";
      device_class = "door";
      value_template = "{{ value_json.open }}";
    }
    {
      platform = "rest";
      resource = "https://status.raumzeitlabor.de/api/full.json";
      method = "GET";
      name = "Door RZL";
      device_class = "door";
      value_template = "{{ value_json.status }}";
    }
    {
      platform = "rest";
      resource = "https://datenobservatorium.de/";
      method = "GET";
      name = "Door Datenobservatorium";
      device_class = "door";
      value_template = "false";
      scan_interval = 2592000;
    }
    {
      platform = "rest";
      resource = "https://infuanfu.de/";
      method = "GET";
      name = "Door Infuanfu";
      device_class = "door";
      value_template = "false";
      scan_interval = 2592000;
    }
  ];
}

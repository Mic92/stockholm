{
  binary_sensor = [
    { platform = "mqtt";
      name = "Portal Lock";
      device_class = "door";
      state_topic = "portal/gateway/status";
      availability_topic = "portal/gateway/lwt";
      payload_on = "open";
      payload_off = "closed";
      payload_available = "online";
      payload_not_available = "offline";
    }
  ];
  sensor = [
    { platform = "mqtt";
      name = "Keyholder";
      state_topic = "portal/gateway/keyholder";
      availability_topic = "portal/gateway/lwt";
      payload_available = "online";
      payload_not_available = "offline";
    }
  ];
}

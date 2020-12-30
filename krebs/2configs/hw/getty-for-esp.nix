{
  # 1. Program an esp8266 devboard (esp8266+usb-ttl) with # https://github.com/jeelabs/esp-link 
  #   tested vesion: esp-link v3.2.47-g9c6530d
  #   Pin Preset: esp-bridge
  #     tx-enable: false
  #     uart-pins: normal
  # 2. connect directly with usb-cable to device, check that vendorID and ProductID match 
  # 3. nc <esp-link-ip> 23
  # Info: for puyak the root pw is `brain hosts/puyak/root`
  services.udev.extraRules = ''
    SUBSYSTEM=="tty", ATTRS{idVendor}=="1a86", ATTRS{idProduct}=="7523", SYMLINK+="ilo", MODE="0660"
  '';
  systemd.services."serial-getty@ilo".enable = true;
  systemd.services."serial-getty@ilo".wantedBy = [ "multi-user.target" ];
  systemd.services."serial-getty@ilo".serviceConfig.Restart = "always";
}


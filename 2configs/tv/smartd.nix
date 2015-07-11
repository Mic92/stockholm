{ config, pkgs, ... }:

{
  services.smartd = {
    enable = true;
    devices = [
      {
        device = "DEVICESCAN";
        options = toString [
          "-a"
          "-m tv@wu.retiolum"
          "-s (O/../.././09|S/../.././04|L/../../6/05)"
        ];
      }
    ];
  };
}

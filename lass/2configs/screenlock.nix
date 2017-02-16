{ pkgs, config, ... }:

{
  systemd.services.screenlock = {
    before = [ "sleep.target" ];
    wantedBy = [ "sleep.target" ];
    environment = {
      DISPLAY = ":0";
    };
    serviceConfig = {
      SyslogIdentifier = "screenlock";
      ExecStart = "${pkgs.i3lock}/bin/i3lock -i /var/lib/wallpaper/wallpaper -f";
      Type = "forking";
      User = "lass";
    };
  };
}

{ pkgs, ... }:

with builtins;

let
  users = [ "tv" ];
  urxvt = pkgs.rxvt_unicode;
  mkService = user: {
    description = "urxvt terminal daemon";
    wantedBy = [ "multi-user.target" ];
    restartIfChanged = false;
    serviceConfig = {
      Restart = "always";
      User = user;
      ExecStart = "${urxvt}/bin/urxvtd";
    };
  };

in

{
  environment.systemPackages = [ urxvt ];
  systemd.services = listToAttrs (map (u: { name = "${u}-urxvtd"; value = mkService u; }) users);
}

{ config, ... }:

{
  services.privoxy = {
    enable = true;
    extraConfig = ''
      #use polipo
      forward / localhost:8123
    '';
  };
  services.polipo.enable = true;
}

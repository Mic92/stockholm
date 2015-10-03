{ config, ... }:

{
  services.privoxy = {
    enable = true;
    extraConfig = ''
      #use polipo
      forward / localhost:8123

      #route .onion through tor
      forward-socks4a .onion localhost:9050
    '';
  };
  services.polipo.enable = true;
}

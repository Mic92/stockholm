{lib, ... }:
{
  services.prometheus.exporters.unifi = {
    enable = true;
    unifiAddress = "https://unifi.shack:8443/";
    unifiInsecure = true;
    unifiUsername = "prometheus"; # needed manual login after setup to confirm the password
    unifiPassword = lib.replaceStrings ["\n"] [""] (builtins.readFile <secrets/shack/unifi-prometheus-pw>);
  };
}

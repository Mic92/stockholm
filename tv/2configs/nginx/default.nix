with import ./lib;
{ config, ... }: {
  services.nginx = {
    enableReload = true;

    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedTlsSettings = true;

    virtualHosts.${toJSON ""} = {
      default = true;
      extraConfig = ''
        error_page 400 =444 /;
        return 444;
      '';
      rejectSSL = true;
    };
  };
  tv.iptables = {
    input-retiolum-accept-tcp = singleton "http";
  };
}

{ config, pkgs, ... }:
{
  services.nginx = {
    virtualHosts.krebs-pages = {
      serverAliases = [
        "krebs.${config.krebs.build.host.name}.r"
      ];
      extraConfig = ''
        root ${pkgs.krebs-pages};
      '';
    };
  };
}

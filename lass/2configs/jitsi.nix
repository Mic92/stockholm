{ config, lib, pkgs, ... }:
{

  services.jitsi-meet = {
    enable = true;
    hostName = "jitsi.lassul.us";
    config = {
      enableWelcomePage = true;
      requireDisplayName = true;
      analytics.disabled = true;
    };
    interfaceConfig = {
      SHOW_JITSI_WATERMARK = false;
      SHOW_WATERMARK_FOR_GUESTS = false;
      DISABLE_PRESENCE_STATUS = true;
      GENERATE_ROOMNAMES_ON_WELCOME_PAGE = false;
    };
  };

  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-p tcp --dport 4443"; target = "ACCEPT"; }
    { predicate = "-p udp --dport 10000"; target = "ACCEPT"; }
  ];
}

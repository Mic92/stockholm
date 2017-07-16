{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

{
  users.extraUsers = {
    download = {
      name = "download";
      home = "/var/download";
      createHome = true;
      useDefaultShell = true;
      extraGroups = [
        "download"
      ];
      openssh.authorizedKeys.keys = with config.krebs.users; [
        lass.pubkey
        lass-shodan.pubkey
        lass-icarus.pubkey
        makefu.pubkey
        wine-mors.pubkey
      ];
    };

    transmission = {
      extraGroups = [
        "download"
      ];
    };
  };

  users.extraGroups = {
    download = {
      members = [
        "download"
        "transmission"
      ];
    };
  };

  krebs.rtorrent = {
    enable = true;
    web = {
      enable = true;
      port = 9091;
      basicAuth = import <secrets/torrent-auth>;
    };
    rutorrent.enable = true;
    enableXMLRPC = true;
    listenPort = 51413;
    downloadDir = "/var/download/finished";
    # dump old torrents into watch folder to have them re-added
    watchDir = "/var/download/watch";
  };

  krebs.iptables = {
    enable = true;
    tables.filter.INPUT.rules = [
      { predicate = "-p tcp --dport 9091"; target = "ACCEPT"; }
      { predicate = "-p tcp --dport 51413"; target = "ACCEPT"; }
      { predicate = "-p udp --dport 51413"; target = "ACCEPT"; }
    ];
  };
}

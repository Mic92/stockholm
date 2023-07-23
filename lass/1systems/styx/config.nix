{ config, pkgs, ... }:

with import <stockholm/lib>;
{
  imports = [
    <stockholm/lass>

    <stockholm/lass/2configs/mouse.nix>
    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/baseX.nix>
    <stockholm/lass/2configs/pipewire.nix>
    <stockholm/lass/2configs/exim-retiolum.nix>
    <stockholm/lass/2configs/browsers.nix>
    <stockholm/lass/2configs/programs.nix>
    <stockholm/lass/2configs/nfs-dl.nix>
    <stockholm/lass/2configs/yellow-mounts/samba.nix>
    <stockholm/lass/2configs/gg23.nix>
    <stockholm/lass/2configs/hass>
    <stockholm/lass/2configs/green-host.nix>
    <stockholm/krebs/2configs/news-host.nix>
    # <stockholm/lass/2configs/br.nix>
    <stockholm/lass/2configs/fetchWallpaper.nix>
    <stockholm/lass/2configs/home-media.nix>
    <stockholm/lass/2configs/syncthing.nix>
    <stockholm/lass/2configs/sync/sync.nix>
    # <stockholm/lass/2configs/idc.nix>
    <stockholm/lass/2configs/ppp/umts-stick.nix>
    <stockholm/lass/2configs/snapserver.nix>
    <stockholm/lass/2configs/snapclient.nix>
  ];

  krebs.build.host = config.krebs.hosts.styx;

  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-p tcp --dport ${toString config.services.smokeping.port}"; target = "ACCEPT"; }
  ];
  krebs.power-action.enable = mkForce false;

  services.smokeping = {
    enable = true;
    targetConfig = ''
      probe = FPing
      menu = top
      title = top

      + Local
      menu = Local
      title = Local Network
      ++ LocalMachine
      menu = Local Machine
      title = This host
      host = localhost

      + Internet
      menu = internet
      title = internet

      ++ CloudflareDNS
      menu = Cloudflare DNS
      title = Cloudflare DNS server
      host = 1.1.1.1

      ++ GoogleDNS
      menu = Google DNS
      title = Google DNS server
      host = 8.8.8.8

      + retiolum
      menu = retiolum
      title = retiolum

      ++ gum
      menu = gum.r
      title = gum.r
      host = gum.r

      ++ ni
      menu = ni.r
      title = ni.r
      host = ni.r

      ++ prism
      menu = prism.r
      title = prism.r
      host = prism.r
    '';
  };
}


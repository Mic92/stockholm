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
    # <stockholm/lass/2configs/idc.nix>
    <stockholm/lass/2configs/ppp/umts-stick.nix>
    <stockholm/lass/2configs/snapserver.nix>
    <stockholm/lass/2configs/snapclient.nix>
    <stockholm/lass/2configs/consul.nix>
  ];

  krebs.build.host = config.krebs.hosts.styx;

  networking.firewall.interfaces.int0.allowedTCPPorts = [ config.services.smokeping.port ];
  networking.firewall.interfaces.retiolum.allowedTCPPorts = [ config.services.smokeping.port ];
  networking.firewall.interfaces.wiregrill.allowedTCPPorts = [ config.services.smokeping.port ];
  krebs.power-action.enable = mkForce false;

  environment.systemPackages = with pkgs; [
    wol
    (writeDashBin "wake-alien" ''
      ${wol}/bin/wol -h 10.42.0.255 10:65:30:68:83:a3
    '')
    (writers.writeDashBin "iptv" ''
      set -efu
      /run/current-system/sw/bin/mpv \
        --audio-display=no --audio-channels=stereo \
        --audio-samplerate=48000 --audio-format=s16 \
        --ao-pcm-file=/run/snapserver/snapfifo --ao=pcm \
        --audio-delay=-1 \
        --playlist=https://iptv-org.github.io/iptv/index.nsfw.m3u \
        --idle=yes \
        --input-ipc-server=/tmp/mpv.ipc \
        "$@"
    '')
  ];

  users.users.mainUser.openssh.authorizedKeys.keys = [
    config.krebs.users.lass-android.pubkey
  ];
  # http://10.42.0.1:8081/smokeping.fcgi
  services.smokeping = {
    enable = true;
    host = null;
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

  # for usb internet
  hardware.usbWwan.enable = true;
}


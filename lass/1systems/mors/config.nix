{ config, lib, pkgs, ... }:

with import <stockholm/lib>;
{
  imports = [
    <stockholm/lass>

    <stockholm/lass/2configs/mouse.nix>
    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/baseX.nix>
    <stockholm/lass/2configs/pipewire.nix>
    <stockholm/lass/2configs/exim-retiolum.nix>
    <stockholm/lass/2configs/programs.nix>
    <stockholm/lass/2configs/bitcoin.nix>
    <stockholm/lass/2configs/browsers.nix>
    <stockholm/lass/2configs/games.nix>
    <stockholm/lass/2configs/pass.nix>
    <stockholm/lass/2configs/elster.nix>
    <stockholm/lass/2configs/steam.nix>
    <stockholm/lass/2configs/wine.nix>
    <stockholm/lass/2configs/fetchWallpaper.nix>
    <stockholm/lass/2configs/mail.nix>
    <stockholm/lass/2configs/logf.nix>
    <stockholm/lass/2configs/syncthing.nix>
    <stockholm/lass/2configs/sync/sync.nix>
    <stockholm/lass/2configs/sync/decsync.nix>
    <stockholm/lass/2configs/sync/weechat.nix>
    <stockholm/lass/2configs/sync/the_playlist.nix>
    #<stockholm/lass/2configs/c-base.nix>
    <stockholm/lass/2configs/br.nix>
    <stockholm/lass/2configs/ableton.nix>
    <stockholm/lass/2configs/dunst.nix>
    <stockholm/lass/2configs/rtl-sdr.nix>
    <stockholm/lass/2configs/print.nix>
    <stockholm/lass/2configs/network-manager.nix>
    <stockholm/lass/2configs/yellow-mounts/samba.nix>
    <stockholm/lass/2configs/ppp/x220-modem.nix>
    <stockholm/lass/2configs/ppp/umts-stick.nix>
    # <stockholm/lass/2configs/remote-builder/morpheus.nix>
    # <stockholm/lass/2configs/remote-builder/prism.nix>
    <stockholm/lass/2configs/consul.nix>
    <stockholm/lass/2configs/networkd.nix>
    <stockholm/lass/2configs/autotether.nix>
    {
      krebs.iptables.tables.filter.INPUT.rules = [
        #risk of rain
        { predicate = "-p tcp --dport 11100"; target = "ACCEPT"; }
        #quake3
        { predicate = "-p tcp --dport 27950:27965"; target = "ACCEPT"; }
        { predicate = "-p udp --dport 27950:27965"; target = "ACCEPT"; }
      ];
    }
    {
      services.nginx = {
        enable = true;
        virtualHosts.default = {
          default = true;
          serverAliases = [
            "localhost"
            "${config.krebs.build.host.name}"
            "${config.krebs.build.host.name}.r"
          ];
          locations."~ ^/~(.+?)(/.*)?\$".extraConfig = ''
            alias /home/$1/public_html$2;
          '';
        };
      };
    }
    {
      services.redis.enable = true;
    }
    {
      environment.systemPackages = [
        pkgs.ovh-zone
        pkgs.bank
        pkgs.adb-sync
        pkgs.transgui
      ];
    }
    {
      services.tor = {
        enable = true;
        client.enable = true;
      };
    }
  ];

  krebs.build.host = config.krebs.hosts.mors;

  environment.systemPackages = with pkgs; [
    acronym
    brain
    cac-api
    sshpass
    get
    hashPassword
    urban
    mk_sql_pair
    remmina
    transmission

    macchanger

    dnsutils
    woeusb
    (pkgs.writeDashBin "play-on" ''
      HOST=$(echo 'styx\nshodan' | fzfmenu)
      ssh -t "$HOST" -- mpv "$@"
    '')
  ];

  #TODO: fix this shit
  ##fprint stuff
  ##sudo fprintd-enroll $USER to save fingerprints
  #services.fprintd.enable = true;
  #security.pam.services.sudo.fprintAuth = true;

  users.extraGroups = {
    loot = {
      members = [
        config.users.extraUsers.mainUser.name
        "firefox"
        "chromium"
        "google"
        "virtual"
      ];
    };
  };

  krebs.repo-sync.timerConfig = {
    OnCalendar = "00:37";
  };

  nixpkgs.config.android_sdk.accept_license = true;
  programs.adb.enable = true;


  services.earlyoom = {
    enable = true;
    freeMemThreshold = 5;
  };


  # It may leak your data, but look how FAST it is!1!!
  # https://make-linux-fast-again.com/
  boot.kernelParams = [
    "noibrs"
    "noibpb"
    "nopti"
    "nospectre_v2"
    "nospectre_v1"
    "l1tf=off"
    "nospec_store_bypass_disable"
    "no_stf_barrier"
    "mds=off"
    "mitigations=off"
  ];

  boot.binfmt.emulatedSystems = [
    "aarch64-linux"
  ];

  nix.trustedUsers = [ "root" "lass" ];

  services.nscd.enableNsncd = true;

}

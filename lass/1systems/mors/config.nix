{ config, pkgs, ... }:

with import <stockholm/lib>;
{
  imports = [
    <stockholm/lass>

    <stockholm/lass/2configs/mouse.nix>
    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/baseX.nix>
    <stockholm/lass/2configs/exim-retiolum.nix>
    <stockholm/lass/2configs/programs.nix>
    <stockholm/lass/2configs/bitcoin.nix>
    <stockholm/lass/2configs/browsers.nix>
    <stockholm/lass/2configs/games.nix>
    <stockholm/lass/2configs/pass.nix>
    <stockholm/lass/2configs/elster.nix>
    <stockholm/lass/2configs/steam.nix>
    <stockholm/lass/2configs/wine.nix>
    <stockholm/lass/2configs/git.nix>
    <stockholm/lass/2configs/fetchWallpaper.nix>
    <stockholm/lass/2configs/mail.nix>
    <stockholm/krebs/2configs/ircd.nix>
    <stockholm/lass/2configs/logf.nix>
    <stockholm/lass/2configs/syncthing.nix>
    <stockholm/lass/2configs/sync/sync.nix>
    <stockholm/lass/2configs/sync/decsync.nix>
    <stockholm/lass/2configs/sync/weechat.nix>
    #<stockholm/lass/2configs/c-base.nix>
    <stockholm/lass/2configs/br.nix>
    <stockholm/lass/2configs/ableton.nix>
    <stockholm/lass/2configs/dunst.nix>
    <stockholm/lass/2configs/rtl-sdr.nix>
    <stockholm/lass/2configs/print.nix>
    <stockholm/lass/2configs/network-manager.nix>
    <stockholm/lass/2configs/nfs-dl.nix>
    <stockholm/lass/2configs/green-host.nix>
    <stockholm/krebs/2configs/news-host.nix>
    <stockholm/lass/2configs/ppp/x220-modem.nix>
    <stockholm/lass/2configs/ppp/umts-stick.nix>
    # <stockholm/lass/2configs/remote-builder/morpheus.nix>
    # <stockholm/lass/2configs/remote-builder/prism.nix>
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
      services.syncthing.declarative = {
        devices.schasch.addresses = [ "schasch.r:22000" ];
        folders = {
          the_playlist = {
            path = "/home/lass/tmp/the_playlist";
            devices = [ "mors" "phone" "prism" "xerxes" ];
          };
          free_music = {
            id = "mu9mn-zgvsw";
            path = "/home/lass/tmp/free_music";
            devices = [ "mors" "schasch" ];
          };
        };
      };
      krebs.permown = {
        "/home/lass/tmp/free_music" = {
          owner = "lass";
          group = "syncthing";
          umask = "0007";
        };
        "/home/lass/tmp/the_playlist" = {
          owner = "lass";
          group = "syncthing";
          umask = "0007";
        };
      };
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
    dpass

    dnsutils
    woeusb
    l-gen-secrets
    generate-secrets
    (pkgs.writeDashBin "btc-coinbase" ''
      ${pkgs.curl}/bin/curl -Ss 'https://api.coinbase.com/v2/prices/spot?currency=EUR' | ${pkgs.jq}/bin/jq '.data.amount'
    '')
    (pkgs.writeDashBin "btc-wex" ''
      ${pkgs.curl}/bin/curl -Ss 'https://wex.nz/api/3/ticker/btc_eur' | ${pkgs.jq}/bin/jq '.btc_eur.avg'
    '')
    (pkgs.writeDashBin "btc-kraken" ''
      ${pkgs.curl}/bin/curl -Ss  'https://api.kraken.com/0/public/Ticker?pair=BTCEUR' | ${pkgs.jq}/bin/jq '.result.XXBTZEUR.a[0]'
    '')
    (pkgs.writeDashBin "krebsco.de" ''
      TMPDIR=$(${pkgs.coreutils}/bin/mktemp -d)
      ${pkgs.brain}/bin/brain show krebs-secrets/ovh-secrets.json > "$TMPDIR"/ovh-secrets.json
      OVH_ZONE_CONFIG="$TMPDIR"/ovh-secrets.json ${pkgs.krebszones}/bin/krebszones import
      ${pkgs.coreutils}/bin/rm -rf "$TMPDIR"
    '')
    (pkgs.writeDashBin "lassul.us" ''
      TMPDIR=$(${pkgs.coreutils}/bin/mktemp -d)
      ${pkgs.pass}/bin/pass show admin/ovh/api.config > "$TMPDIR"/ovh-secrets.json
      OVH_ZONE_CONFIG="$TMPDIR"/ovh-secrets.json ${pkgs.ovh-zone}/bin/ovh-zone import /etc/zones/lassul.us lassul.us
      ${pkgs.coreutils}/bin/rm -rf "$TMPDIR"
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
  users.users.mainUser.extraGroups = [ "adbusers" "docker" ];
  virtualisation.docker.enable = true;

  virtualisation.libvirtd.enable = true;

  services.earlyoom = {
    enable = true;
    freeMemThreshold = 5;
  };
}

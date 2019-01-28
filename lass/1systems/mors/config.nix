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
    <stockholm/lass/2configs/virtualbox.nix>
    <stockholm/lass/2configs/fetchWallpaper.nix>
    <stockholm/lass/2configs/mail.nix>
    <stockholm/krebs/2configs/ircd.nix>
    <stockholm/lass/2configs/logf.nix>
    <stockholm/lass/2configs/syncthing.nix>
    <stockholm/lass/2configs/otp-ssh.nix>
    <stockholm/lass/2configs/c-base.nix>
    <stockholm/lass/2configs/br.nix>
    <stockholm/lass/2configs/ableton.nix>
    <stockholm/lass/2configs/starcraft.nix>
    <stockholm/lass/2configs/dunst.nix>
    <stockholm/lass/2configs/rtl-sdr.nix>
    <stockholm/lass/2configs/backup.nix>
    <stockholm/lass/2configs/print.nix>
    <stockholm/lass/2configs/blue-host.nix>
    <stockholm/lass/2configs/network-manager.nix>
    <stockholm/lass/2configs/nfs-dl.nix>
    <stockholm/lass/2configs/hardening.nix>
    {
      krebs.iptables.tables.filter.INPUT.rules = [
        #risk of rain
        { predicate = "-p tcp --dport 11100"; target = "ACCEPT"; }
        #chromecast
        { predicate = "-p udp -m multiport --sports 32768:61000 -m multiport --dports 32768:61000"; target = "ACCEPT"; }
        #quake3
        { predicate = "-p tcp --dport 27950:27965"; target = "ACCEPT"; }
        { predicate = "-p udp --dport 27950:27965"; target = "ACCEPT"; }
      ];
    }
    {
      lass.umts = {
        enable = true;
        modem = "/dev/serial/by-id/usb-Lenovo_F5521gw_2C7D8D7C35FC7040-if09";
        initstrings = ''
          Init1 = AT+CFUN=1
          Init2 = AT+CGDCONT=1,"IP","pinternet.interkom.de","",0,0
        '';
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
    teamspeak_client
    hashPassword
    urban
    mk_sql_pair
    remmina
    transmission

    iodine

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

  lass.restic = genAttrs [
    "daedalus"
    "icarus"
    "littleT"
    "prism"
    "shodan"
    "skynet"
  ] (dest: {
    dirs = [
      "/home/lass/src"
      "/home/lass/work"
      "/home/lass/.gnupg"
      "/home/lass/Maildir"
      "/home/lass/stockholm"
      "/home/lass/.password-store"
      "/home/bitcoin"
      "/home/bch"
    ];
    passwordFile = (toString <secrets>) + "/restic/${dest}";
    repo = "sftp:backup@${dest}.r:/backups/mors";
    #sshPrivateKey = config.krebs.build.host.ssh.privkey.path;
    extraArguments = [
      "sftp.command='ssh backup@${dest}.r -i ${config.krebs.build.host.ssh.privkey.path} -s sftp'"
    ];
    timerConfig = {
      OnCalendar = "00:05";
      RandomizedDelaySec = "5h";
    };
  });
  virtualisation.libvirtd.enable = true;

  services.earlyoom = {
    enable = true;
    freeMemThreshold = 5;
  };
}

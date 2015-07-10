{ config, pkgs, ... }:

let
  location = pkgs.lib.nameValuePair; # TODO this is also in modules/tv/git/cgit.nix
in

{
  imports = [
    ./hosts.nix
    ../tv/base.nix
    ../tv/exim-retiolum.nix
    ../tv/environment.nix
    ../tv/sanitize.nix
    ../tv/smartd.nix
    ../tv/synaptics.nix
    ../tv/urxvt.nix
    ../tv/xserver.nix
    ../wu/users.nix
    {
      imports = [ ../tv/iptables ];
      tv.iptables = {
        enable = true;
        input-internet-accept-new-tcp = [
          "ssh"
          "http"
          "tinc"
          "smtp"
        ];
      };
    }
    {
      imports = [ ../tv/nginx ];
      tv.nginx = {
        enable = true;
        retiolum-locations = [
          (location "~ ^/~(.+?)(/.*)?\$" ''
            alias /home/$1/public_html$2;
          '')
        ];
      };
    }
    {
      imports = [ ../tv/retiolum ];
      tv.retiolum = {
        enable = true;
        hosts = <retiolum-hosts>;
        connectTo = [
          "gum"
          "pigstarter"
        ];
      };
    }
    {
      imports = [ ../tv/urlwatch ];
      tv.urlwatch = {
        enable = true;
        mailto = "tv@wu.retiolum";
        onCalendar = "*-*-* 05:00:00";
        urls = [
          ## nixpkgs maintenance

          # 2014-07-29 when one of the following urls change
          # then we have to update the package

          # ref src/nixpkgs/pkgs/tools/admin/sec/default.nix
          http://simple-evcorr.sourceforge.net/

          # ref src/nixpkgs/pkgs/tools/networking/urlwatch/default.nix
          https://thp.io/2008/urlwatch/

          # 2014-12-20 ref src/nixpkgs/pkgs/tools/networking/tlsdate/default.nix
          https://api.github.com/repos/ioerror/tlsdate/tags

          # 2015-02-18
          # ref ~/src/nixpkgs/pkgs/tools/text/qprint/default.nix
          http://www.fourmilab.ch/webtools/qprint/

          # 2014-09-24 ref https://github.com/4z3/xintmap
          http://www.mathstat.dal.ca/~selinger/quipper/

          # 2014-12-12 remove nixopsUnstable when nixops get's bumped to 1.3
          # ref https://github.com/NixOS/nixpkgs/blob/master/pkgs/tools/package-management/nixops/unstable.nix
          http://nixos.org/releases/nixops/

          ## other

          https://nixos.org/channels/nixos-unstable/git-revision

          ## 2014-10-17
          ## TODO update ~/src/login/default.nix
          #http://hackage.haskell.org/package/bcrypt
          #http://hackage.haskell.org/package/cron
          #http://hackage.haskell.org/package/hyphenation
          #http://hackage.haskell.org/package/iso8601-time
          #http://hackage.haskell.org/package/ixset-typed
          #http://hackage.haskell.org/package/system-command
          #http://hackage.haskell.org/package/transformers
          #http://hackage.haskell.org/package/web-routes-wai
          #http://hackage.haskell.org/package/web-page
        ];
      };
    }
  ];

  nix = {
    buildCores = 4;
    maxJobs = 4;
    daemonIONiceLevel = 1;
    daemonNiceLevel = 1;
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="00:90:f5:da:aa:c3", NAME="en0"
    SUBSYSTEM=="net", ATTR{address}=="a0:88:b4:1b:ae:6c", NAME="wl0"

    # for jack
    KERNEL=="rtc0", GROUP="audio"
    KERNEL=="hpet", GROUP="audio"
  '';

  #services.virtualbox.enable = true;
  #services.virtualboxGuest.enable = false;
  services.virtualboxHost.enable = true;
  #services.virtualboxHost.addNetworkInterface = false;
  #systemd.services.vboxnet =
  #  let
  #    remove_vboxnets = ''
  #      for i in $(cd /sys/class/net && ls | grep ^vboxnet); do
  #        VBoxManage hostonlyif remove $i
  #      done
  #    '';
  #  in {
  #    wantedBy = [ "multi-user.target" ];
  #    requires = [ "dev-vboxnetctl.device" ];
  #    after = [ "dev-vboxnetctl.device" ];
  #    path = with pkgs; [
  #      linuxPackages.virtualbox
  #      nettools
  #    ];
  #    postStop = remove_vboxnets;
  #    script = ''
  #      ${remove_vboxnets} # just in case...
  #      VBoxManage hostonlyif create # vboxnet0
  #      ifconfig vboxnet0 up 169.254.13.37/16
  #    '';
  #    serviceConfig = {
  #      Type = "oneshot";
  #      PrivateTmp = true;
  #      RemainAfterExit = "yes";
  #    };
  #    environment.VBOX_USER_HOME = "/tmp";
  #  };


  services.bitlbee.enable = true;

  #services.rabbitmq = {
  #  enable = true;
  #  cookie = "f00f";
  #  plugins = [
  #    "rabbitmq_management"
  #  ];
  #};


  #services.elasticsearch.enable = true;

  #services.cgserver = {
  #  enable = true;
  #  httpPort = 8003;
  #  #flushLog = false;
  #  #cgroupRoot = "/sys/fs/cgroup";
  #  #user = "zalora";
  #};




  #services.tlsdated = {
  #  enable = true;
  #  extraOptions = "-p";
  #};

  services.tor.enable = true;
  services.tor.client.enable = true;



  # hardware configuration
  boot.initrd.luks.devices = [
    { name = "home"; device = "/dev/vg840/enchome"; preLVM = false; }
  ];
  boot.initrd.luks.cryptoModules = [ "aes" "sha512" "xts" ];
  boot.initrd.availableKernelModules = [ "ahci" ];
  #boot.kernelParams = [
  #  "intel_pstate=enable"
  #];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  # 2014-12-17 pkgs.linuxPackages_3_14 is known good
  boot.kernelPackages = pkgs.linuxPackages_3_18;

  boot.kernel.sysctl = {
    # Enable IPv6 Privacy Extensions
    "net.ipv6.conf.all.use_tempaddr" = 2;
    "net.ipv6.conf.default.use_tempaddr" = 2;
  };

  boot.extraModprobeConfig = ''
    options kvm_intel nested=1
  '';

  fileSystems = {
    "/" = {
      device = "/dev/mapper/vg840-wuroot";
      fsType = "btrfs";
      options = "defaults,noatime,ssd,compress=lzo";
    };
    "/home" = {
      device = "/dev/mapper/home";
      options = "defaults,noatime,ssd,compress=lzo";
    };
    "/boot" = {
      device = "/dev/sda1";
    };
    "/tmp" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = "nosuid,nodev,noatime";
    };
  };

  swapDevices =[ ];


  nixpkgs.config.firefox.enableAdobeFlash = true;
  nixpkgs.config.chromium.enablePepperFlash = true;

  nixpkgs.config.allowUnfree = true;
  hardware.bumblebee.enable = true; # TODO this is host specific
  hardware.bumblebee.group = "video";
  #services.xserver.videoDrivers = [ "nvidia" ];
  hardware.opengl.driSupport32Bit = true;

  hardware.pulseaudio.enable = true;

  hardware.enableAllFirmware = true;

  # Use the gummiboot efi boot loader.
  boot.loader.gummiboot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "wu";
  networking.wireless.enable = true;


  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "lat9w-16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  system.activationScripts.powertopTunables =
    ''
      echo 1 > /sys/module/snd_hda_intel/parameters/power_save
      echo 1500 > /proc/sys/vm/dirty_writeback_centisecs
      (cd /sys/bus/pci/devices
        for i in *; do
          echo auto > $i/power/control # defaults to 'on'
        done)
      # TODO maybe do this via udev or systemd
      #   ref https://wiki.archlinux.org/index.php/Wake-on-LAN
      # disable wol this cannot find ethtool
      # TODO (cd /sys/class/net
      # TODO   for i in *; do
      # TODO     if ethtool $i | grep -q Wake-on &&
      # TODO         ! ethtool $i | grep -q 'Wake-on: d'; then
      # TODO       ethtool -s $i wol d
      # TODO     fi
      # TODO   done)
      ${pkgs.ethtool}/sbin/ethtool -s en0 wol d
    '';

  environment.systemPackages = with pkgs; [
    xlibs.fontschumachermisc
    slock
    ethtool
    #firefoxWrapper # with plugins
    #chromiumDevWrapper
    tinc
    iptables
    #jack2
  ];

  security.setuidPrograms = [
    "sendmail"  # for cron
    "slock"
  ];

  # TODO
  # Currently ./run doesn't know about certificates
  #security.pki.certificateFiles = [
  #  ./certs/zalora-ca.crt
  #];

  #security.pam.loginLimits = [
  #  # for jack
  #  { domain = "@audio"; item = "memlock"; type = "-"; value = "unlimited"; }
  #  { domain = "@audio"; item = "rtprio"; type = "-"; value = "99"; }
  #];

  #services.haveged.enable = true;
  #security.rngd.enable = true;

  #services.privoxy = {
  #  enable = true;
  #  extraConfig = ''
  #    actionsfile /etc/privoxy/easylist.script.action
  #    actionsfile /etc/privoxy/easylistgermany.script.action
  #    filterfile /etc/privoxy/easylist.script.filter
  #    filterfile /etc/privoxy/easylistgermany.script.filter
  #  '';
  #};

  #services.dbus.enable = true; # rqd4 wpa_supplicant

  services.logind.extraConfig = ''
    HandleHibernateKey=ignore
    HandleLidSwitch=ignore
    HandlePowerKey=ignore
    HandleSuspendKey=ignore
  '';

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    hostKeys = [
      # XXX bits here make no science
      { bits = 8192; type = "ed25519"; path = "/etc/ssh/ssh_host_ed25519_key"; }
    ];
  };

  # services.printing.enable = true;
  services.printing = {
    enable = true;
    #extraConf = ''
    #  LogLevel debug
    #'';
    drivers = with pkgs; [
      #cups_filters
      #foomatic_filters
      #gutenprint
      #hplip
    ];
  };



  #services.kmscon.enable = true;


  # TODO virtualisation.libvirtd.enable = true;
  #       users.extraUsers.tv.extraGroups += [ "libvirtd" ]




  services.journald.extraConfig = ''
    SystemMaxUse=1G
    RuntimeMaxUse=128M
  '';


  #systemd.timers.chargeMon = {
  #  wantedBy = [ "multi-user.target" ];
  #  timerConfig.OnCalendar = "*-*-* *:*:00";
  #};
  #systemd.services.chargeMon = {
  #  path  = [ ];
  #  environment = {
  #    ac_online   = "/sys/class/power_supply/AC/online";
  #    charge_now  = "/sys/class/power_supply/BAT/charge_now";
  #    charge_full = "/sys/class/power_supply/BAT/charge_full";
  #  };
  #  serviceConfig = {
  #    User = "nobody";
  #    Type = "oneshot";
  #  };
  #  script = ''
  #    if test $(cat $ac_online) == 1; then
  #      echo "AC is online"
  #      exit
  #    fi
  #    cat $charge_now
  #  '';
  #};

  # see tmpfiles.d(5)
  systemd.tmpfiles.rules = [
    "d /tmp 1777 root root - -" # does this work with mounted /tmp?
  ];

  # TODO services.smartd
  # TODO services.statsd
  # TODO services.tor
  # TODO write arandr
  # TODO what does system.copySystemConfiguration (we need some kind of bku scheme)
  # TODO systemd.timers instead of cron(??)

  virtualisation.libvirtd.enable = true;




  #system.replaceRuntimeDependencies = with pkgs;
  #  let
  #      bashVulnPatches = [
  #        (fetchurl {
  #          url = "mirror://gnu/bash/bash-4.2-patches/bash42-048";
  #          sha256 = "091xk1ms7ycnczsl3fx461gjhj69j6ycnfijlymwj6mj60ims6km";
  #        })
  #        (fetchurl {
  #          url = "file:///etc/nixos/bash-20140926.patch";
  #          sha256 = "0gdwnimsbi4vh5l46krss4wjrgbch94skn4y2w3rpvb1w4jypha4";
  #        })
  #      ];
  #  in
  #  [
  #    {
  #      original = bash;
  #      replacement = pkgs.lib.overrideDerivation bash (oldAttrs: {
  #        patches = oldAttrs.patches ++ bashVulnPatches;
  #      });
  #    }
  #    {
  #      original = bashInteractive;
  #      replacement = pkgs.lib.overrideDerivation bashInteractive (oldAttrs: {
  #        patches = oldAttrs.patches ++ bashVulnPatches;
  #      });
  #    }
  #    {
  #      original = bitlbee;
  #      replacement = pkgs.lib.overrideDerivation bitlbee (oldAttrs: {
  #        configureFlags = [
  #          "--gcov=1"
  #          "--otr=1"
  #          "--ssl=gnutls"
  #        ];
  #      });
  #    }
  #];


}

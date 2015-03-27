{ config, pkgs, ... }:

let
  lib = import ./lib { inherit pkgs; };

  inherit (lib) majmin;
in

{
  imports = [
    ./modules/sanitize.nix
    ./modules/base.nix
    ./modules/retiolum.nix
    ./modules/urxvt.nix
    ./modules/iptables.nix
    ./modules/users.nix
    #./modules/tools.nix
    ./modules/hosts.nix
    ./modules/xserver.nix
    ./modules/exim.nix
    ./modules/nginx.nix
  ];

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
    vim
    #jack2
  ];


  environment.etc."vim/vimrc".text = ''
    set nocp
  '';
  environment.etc."vim/vim${majmin pkgs.vim.version}".source =
      "${pkgs.vim}/share/vim/vim${majmin pkgs.vim.version}";

  # multiple-definition-problem when defining environment.variables.EDITOR
  environment.extraInit = ''
    EDITOR=vim
  '';
  environment.variables.VIM = "/etc/vim";

  environment.shellAliases = {
    # alias cal='cal -m3'
    bc = "bc -q";
    gp = "gp -q";
    df = "df -h";
    du = "du -h";
    # alias grep='grep --color=auto'

    # TODO alias cannot contain #\'
    # "ps?" = "ps ax | head -n 1;ps ax | fgrep -v ' grep --color=auto ' | grep";

    # alias la='ls -lA'
    lAtr = "ls -lAtr";
    # alias ll='ls -l'
    ls = "ls -h --color=auto --group-directories-first";
    # alias vim='vim -p'
    # alias vi='vim'
    # alias view='vim -R'
    dmesg = "dmesg -L --reltime";
  };


  programs.bash = {
    interactiveShellInit = ''
      HISTCONTROL='erasedups:ignorespace'
      HISTSIZE=65536
      HISTFILESIZE=$HISTSIZE

      shopt -s checkhash
      shopt -s histappend histreedit histverify
      shopt -s no_empty_cmd_completion
      complete -d cd

      # TODO source bridge
    '';
    promptInit = ''
      case $UID in
        0)
          PS1='\[\e[1;31m\]\w\[\e[0m\] '
          ;;
        1337)
          PS1='\[\e[1;32m\]\w\[\e[0m\] '
          ;;
        *)
          PS1='\[\e[1;35m\]\u \[\e[1;32m\]\w\[\e[0m\] '
          ;;
      esac
      if test -n "$SSH_CLIENT"; then
        PS1='\[\e[35m\]\h'" $PS1"
      fi
      if test -n "$SSH_AGENT_PID"; then
        PS1="ssh-agent[$SSH_AGENT_PID] $PS1"
      fi
    '';
  };


  programs.ssh.startAgent = false;


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

  services.retiolum = {
    enable = true;
    hosts = /etc/nixos/hosts;
    connectTo = [
      "gum"
      "pigstarter"
    ];
  };

  # TODO
  #services.tinc = {
  #  enable = true;
  #  network = "retiolum";
  #  hosts = /home/tv/krebs/hosts;
  #  privateKeyFile = /etc/tinc/retiolum/rsa_key.priv;
  #  connectTo = [ "fastpoke" "pigstarter" "kheurop" ];
  #};


  security.rtkit.enable = false;
  services.nscd.enable = false;
  services.ntp.enable = false;
  #services.dbus.enable = true; # rqd4 wpa_supplicant

  # vixiecron sucks
  services.cron.enable = false;
  services.fcron.enable = true;

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

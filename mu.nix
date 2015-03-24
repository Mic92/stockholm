# TODO maybe give RT-stuff only to group rt or sth.

{ config, pkgs, ... }:

let
  lib = import ./lib { inherit pkgs; };

  inherit (lib) majmin;
in

{
  imports = [
    <secrets/hashedPasswords.nix>
    ./modules/exim.nix
    ./modules/retiolum.nix
  ];


  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="00:90:f5:da:aa:c3", NAME="en0"
    SUBSYSTEM=="net", ATTR{address}=="a0:88:b4:1b:ae:6c", NAME="wl0"

    # for jack
    KERNEL=="rtc0", GROUP="audio"
    KERNEL=="hpet", GROUP="audio"
  '';


  # hardware configuration
  boot.initrd.luks.devices = [
    { name = "vgmu1"; device = "/dev/sda2"; }
  ];
  boot.initrd.luks.cryptoModules = [ "aes" "sha512" "xts" ];
  boot.initrd.availableKernelModules = [ "ahci" ];
  #boot.kernelParams = [
  #  "intel_pstate=enable"
  #];
  boot.kernelModules = [ "fbcon" "kvm-intel" ];
  boot.extraModulePackages = [ ];

  #boot.kernelPackages = pkgs.linuxPackages_3_17;

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
      device = "/dev/vgmu1/nixroot";
      fsType = "ext4";
      options = "defaults,noatime";
    };
    "/home" = {
      device = "/dev/vgmu1/home";
      options = "defaults,noatime";
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

  nix.maxJobs = 8;
  nix.useChroot = true;

  nixpkgs.config.firefox.enableAdobeFlash = true;
  nixpkgs.config.chromium.enablePepperFlash = true;

  nixpkgs.config.allowUnfree = true;
  hardware.opengl.driSupport32Bit = true;

  hardware.pulseaudio.enable = true;

  hardware.enableAllFirmware = true;

  # Use the gummiboot efi boot loader.
  boot.loader.gummiboot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "mu";
  #networking.wireless.enable = true;
  networking.networkmanager.enable = true;

  networking.extraHosts = ''
  '';

  #system.activationScripts.powertopTunables =
  #  ''
  #    #echo 1 > /sys/module/snd_hda_intel/parameters/power_save
  #    echo 1500 > /proc/sys/vm/dirty_writeback_centisecs
  #    (cd /sys/bus/pci/devices
  #      for i in *; do
  #        echo auto > $i/power/control # defaults to 'on'
  #      done)
  #    # TODO maybe do this via udev or systemd
  #    #   ref https://wiki.archlinux.org/index.php/Wake-on-LAN
  #    # disable wol this cannot find ethtool
  #    # TODO (cd /sys/class/net
  #    # TODO   for i in *; do
  #    # TODO     if ethtool $i | grep -q Wake-on &&
  #    # TODO         ! ethtool $i | grep -q 'Wake-on: d'; then
  #    # TODO       ethtool -s $i wol d
  #    # TODO     fi
  #    # TODO   done)
  #    ${pkgs.ethtool}/sbin/ethtool -s en0 wol d
  #  '';

  environment.systemPackages = with pkgs; [
    slock
    tinc
    iptables
    vim
    gimp
    xsane
    firefoxWrapper
    chromiumDev
    skype
    libreoffice
    kde4.l10n.de
    kde4.networkmanagement
    pidgin-with-plugins
    pidginotr

    kde4.print_manager
    #foomatic_filters
    #gutenprint
    #cups_pdf_filter
    #ghostscript
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

  i18n.defaultLocale = "de_DE.UTF-8";

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
        2000)
          PS1='\[\e[1;32m\]\w\[\e[0m\] '
          ;;
        *)
          PS1='\[\e[1;35m\]\u \[\e[1;32m\]\w\[\e[0m\] '
          ;;
      esac
      if test -n "$SSH_CLIENT"; then
        PS1='\[\e[35m\]\h'" $PS1"
      fi
    '';
  };


  programs.ssh.startAgent = false;


  security.setuidPrograms = [
    "sendmail"  # for cron
    "slock"
  ];

  security.pam.loginLimits = [
    # for jack
    { domain = "@audio"; item = "memlock"; type = "-"; value = "unlimited"; }
    { domain = "@audio"; item = "rtprio"; type = "-"; value = "99"; }
  ];

  #services.haveged.enable = true;
  #security.rngd.enable = true;

  services.retiolum = {
    enable = true;
    hosts = /etc/nixos/hosts;
    connectTo = [
      "gum"
      "pigstarter"
    ];
  };

  security.rtkit.enable = false;
  services.nscd.enable = false;
  services.ntp.enable = false;
  #services.dbus.enable = true; # rqd4 wpa_supplicant

  services.sshd.enable = true;

  # vixiecron sucks
  services.cron.enable = false;
  services.fcron.enable = true;

  fonts.fonts = [
    pkgs.xlibs.fontschumachermisc
  ];

  #services.logind.extraConfig = ''
  #  HandleHibernateKey=ignore
  #  HandleLidSwitch=ignore
  #  HandlePowerKey=ignore
  #  HandleSuspendKey=ignore
  #'';
  #services.xserver.displayManager.desktopManagerHandlesLidAndPower = true;

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    hostKeys = [
      # XXX bits here make no science
      { bits = 8192; type = "ed25519"; path = "/etc/ssh/ssh_host_ed25519_key"; }
    ];
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;
  services.printing = {
    enable = true;
    #drivers = [
    #  #pkgs.foomatic_filters
    #  #pkgs.gutenprint
    #  #pkgs.cups_pdf_filter
    #  #pkgs.ghostscript
    #];
    #cupsdConf = ''
    #  LogLevel debug2
    #'';
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  #services.xserver.display = 11;
  #services.xserver.tty = 11;
  services.xserver.layout = "de";
  services.xserver.xkbOptions = "eurosign:e";

  # TODO this is host specific
  services.xserver.synaptics = {
    enable = true;
    twoFingerScroll = true;
    #accelFactor = "0.035";
    #additionalOptions = ''
    #  Option "FingerHigh" "60"
    #  Option "FingerLow"  "60"
    #'';
  };

  services.xserver.desktopManager.kde4.enable = true;
  services.xserver.displayManager.auto = {
    enable = true;
    user = "vv";
  };

  users.defaultUserShell = "/run/current-system/sw/bin/bash";
  users.mutableUsers = false;
  users.extraGroups =
    {
    };
  users.extraUsers =
    {
      tv = {
        uid = 1337;
        name = "tv";
        group = "users";
        home = "/home/tv";
        useDefaultShell = true;
        extraGroups = [
          "audio"
          "video"
          "wheel"
        ];
        createHome = true;
      };

      vv = {
        uid = 2000;
        name = "vv";
        home = "/home/vv";
        createHome = true;
        group = "users";
        useDefaultShell = true;
        extraGroups = [
          "audio"
          "video"
          "networkmanager"
        ];
      };
    };

  services.journald.extraConfig = ''
    SystemMaxUse=1G
    RuntimeMaxUse=128M
  '';

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

  #
  # iptables
  #
  networking.firewall.enable = false;
  system.activationScripts.iptables =
    let
      log = false;
      when = c: f: if c then f else "";
    in
      ''
        ip4tables() { ${pkgs.iptables}/sbin/iptables "$@"; }
        ip6tables() { ${pkgs.iptables}/sbin/ip6tables "$@"; }
        ipXtables() { ip4tables "$@"; ip6tables "$@"; }

        #
        # nat
        #

        # reset tables
        ipXtables -t nat -F
        ipXtables -t nat -X

        #
        ipXtables -t nat -A PREROUTING -j REDIRECT ! -i retiolum -p tcp --dport ssh --to-ports 0
        ipXtables -t nat -A PREROUTING -j REDIRECT -p tcp --dport 11423 --to-ports ssh

        #
        # filter
        #

        # reset tables
        ipXtables -P INPUT DROP
        ipXtables -P FORWARD DROP
        ipXtables -F
        ipXtables -X

        # create custom chains
        ipXtables -N Retiolum

        # INPUT
        ipXtables -A INPUT -j ACCEPT -m conntrack --ctstate RELATED,ESTABLISHED
        ipXtables -A INPUT -j ACCEPT -i lo
        ipXtables -A INPUT -j ACCEPT -p tcp --dport ssh -m conntrack --ctstate NEW
        ipXtables -A INPUT -j ACCEPT -p tcp --dport http -m conntrack --ctstate NEW
        ipXtables -A INPUT -j ACCEPT -p tcp --dport tinc -m conntrack --ctstate NEW
        ipXtables -A INPUT -j ACCEPT -p tcp --dport smtp -m conntrack --ctstate NEW
        ipXtables -A INPUT -j Retiolum -i retiolum
        ${when log "ipXtables -A INPUT -j LOG --log-level info --log-prefix 'INPUT DROP '"}

        # FORWARD
        ${when log "ipXtables -A FORWARD -j LOG --log-level info --log-prefix 'FORWARD DROP '"}

        # Retiolum
        ip4tables -A Retiolum -j ACCEPT -p icmp --icmp-type echo-request
        ip6tables -A Retiolum -j ACCEPT -p ipv6-icmp -m icmp6 --icmpv6-type echo-request


        ${when log "ipXtables -A Retiolum -j LOG --log-level info --log-prefix 'REJECT '"}
        ipXtables -A Retiolum -j REJECT -p tcp --reject-with tcp-reset
        ip4tables -A Retiolum -j REJECT -p udp --reject-with icmp-port-unreachable
        ip4tables -A Retiolum -j REJECT        --reject-with icmp-proto-unreachable
        ip6tables -A Retiolum -j REJECT -p udp --reject-with icmp6-port-unreachable
        ip6tables -A Retiolum -j REJECT

      '';




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

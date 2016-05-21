{ config, lib, pkgs, ... }:

with config.krebs.lib;
{
  imports = [
    ../2configs/vim.nix
    ../2configs/zsh.nix
    ../2configs/mc.nix
    ../2configs/retiolum.nix
    ./backups.nix
    {
      users.extraUsers =
        mapAttrs (_: h: { hashedPassword = h; })
                 (import <secrets/hashedPasswords.nix>);
    }
    {
      users.extraUsers = {
        root = {
          openssh.authorizedKeys.keys = [
            config.krebs.users.lass.pubkey
            config.krebs.users.lass-uriel.pubkey
            config.krebs.users.lass-shodan.pubkey
          ];
        };
        mainUser = {
          name = "lass";
          uid = 1337;
          home = "/home/lass";
          group = "users";
          createHome = true;
          useDefaultShell = true;
          extraGroups = [
          ];
          openssh.authorizedKeys.keys = [
            config.krebs.users.lass.pubkey
            config.krebs.users.lass-uriel.pubkey
            config.krebs.users.lass-shodan.pubkey
          ];
        };
      };
    }
  ];

  networking.hostName = config.krebs.build.host.name;
  nix.maxJobs = config.krebs.build.host.cores;

  krebs = {
    enable = true;
    search-domain = "retiolum";
    build = {
      user = config.krebs.users.lass;
      source = mapAttrs (_: mkDefault) ({
        nixos-config = "symlink:stockholm/lass/1systems/${config.krebs.build.host.name}.nix";
        secrets = "/home/lass/secrets/${config.krebs.build.host.name}";
        #secrets-common = "/home/lass/secrets/common";
        stockholm = "/home/lass/stockholm";
        nixpkgs = {
          url = https://github.com/NixOS/nixpkgs;
          rev = "d541e0dc1c05f5514bf30f8039e687adddb45616";
          dev = "/home/lass/src/nixpkgs";
        };
      } // optionalAttrs config.krebs.build.host.secure {
        #secrets-master = "/home/lass/secrets/master";
      });
    };
  };

  nix.useChroot = true;

  users.mutableUsers = false;

  services.timesyncd.enable = true;

  #why is this on in the first place?
  services.nscd.enable = false;

  boot.tmpOnTmpfs = true;
  # see tmpfiles.d(5)
  systemd.tmpfiles.rules = [
    "d /tmp 1777 root root - -"
  ];

  # multiple-definition-problem when defining environment.variables.EDITOR
  environment.extraInit = ''
    EDITOR=vim
    MANPAGER=most
  '';

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
  #stockholm
    git
    gnumake
    jq
    parallel
    proot

  #style
    most
    rxvt_unicode.terminfo

  #monitoring tools
    htop
    iotop

  #network
    iptables
    iftop

  #stuff for dl
    aria2

  #neat utils
    krebspaste
    psmisc
    untilport

  #unpack stuff
    p7zip
    unzip
    unrar
  ];

  programs.bash = {
    enableCompletion = true;
    interactiveShellInit = ''
      HISTCONTROL='erasedups:ignorespace'
      HISTSIZE=65536
      HISTFILESIZE=$HISTSIZE

      shopt -s checkhash
      shopt -s histappend histreedit histverify
      shopt -s no_empty_cmd_completion
      complete -d cd

      #fancy colors
      if [ -e ~/LS_COLORS ]; then
        eval $(dircolors ~/LS_COLORS)
      fi

      if [ -e /etc/nixos/dotfiles/link ]; then
        /etc/nixos/dotfiles/link
      fi
    '';
    promptInit = ''
      if test $UID = 0; then
        PS1='\[\033[1;31m\]\w\[\033[0m\] '
      elif test $UID = 1337; then
        PS1='\[\033[1;32m\]\w\[\033[0m\] '
      else
        PS1='\[\033[1;33m\]\u@\w\[\033[0m\] '
      fi
      if test -n "$SSH_CLIENT"; then
        PS1='\[\033[35m\]\h'" $PS1"
      fi
    '';
  };

  services.openssh = {
    enable = true;
    hostKeys = [
      # XXX bits here make no science
      { bits = 8192; type = "ed25519"; path = "/etc/ssh/ssh_host_ed25519_key"; }
    ];
  };

  services.journald.extraConfig = ''
    SystemMaxUse=1G
    RuntimeMaxUse=128M
  '';

  krebs.iptables = {
    enable = true;
    tables = {
      nat.PREROUTING.rules = [
        { predicate = "! -i retiolum -p tcp -m tcp --dport 22"; target = "REDIRECT --to-ports 0"; precedence = 100; }
        { predicate = "-p tcp -m tcp --dport 45621"; target = "REDIRECT --to-ports 22"; precedence = 99; }
      ];
      nat.OUTPUT.rules = [
        { predicate = "-o lo -p tcp -m tcp --dport 45621"; target = "REDIRECT --to-ports 22"; precedence = 100; }
      ];
      filter.INPUT.policy = "DROP";
      filter.FORWARD.policy = "DROP";
      filter.INPUT.rules = [
        { predicate = "-m conntrack --ctstate RELATED,ESTABLISHED"; target = "ACCEPT"; precedence = 10001; }
        { predicate = "-p icmp"; target = "ACCEPT"; precedence = 10000; }
        { predicate = "-i lo"; target = "ACCEPT"; precedence = 9999; }
        { predicate = "-p tcp --dport 22"; target = "ACCEPT"; precedence = 9998; }
        { predicate = "-i retiolum"; target = "REJECT"; precedence = -10000; }
      ];
    };
  };

  networking.dhcpcd.extraConfig = ''
    noipv4ll
  '';

  #CVE-2016-0777 and CVE-2016-0778 workaround
  #https://www.qualys.com/2016/01/14/cve-2016-0777-cve-2016-0778/openssh-cve-2016-0777-cve-2016-0778.txt
  programs.ssh.extraConfig = ''
    UseRoaming no
  '';

}

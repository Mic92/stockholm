{ config, lib, pkgs, ... }:

with lib;
{
  imports = [
    ../2configs/vim.nix
    ../2configs/zsh.nix
    ../2configs/mc.nix
    ../2configs/retiolum.nix
    {
      users.extraUsers =
        mapAttrs (_: h: { hashedPassword = h; })
                 (import /root/secrets/hashedPasswords.nix);
    }
    {
      users.extraUsers = {
        root = {
          openssh.authorizedKeys.keys = map readFile [
            ../../krebs/Zpubkeys/lass.ssh.pub
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
            "audio"
            "wheel"
          ];
          openssh.authorizedKeys.keys = map readFile [
            ../../krebs/Zpubkeys/lass.ssh.pub
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
    exim-retiolum.enable = true;
    build = {
      user = config.krebs.users.lass;
      source = {
        git.nixpkgs = {
          url = https://github.com/Lassulus/nixpkgs;
          rev = "33bdc011f5360288cd10b9fda90da2950442b2ab";
        };
        dir.secrets = {
          host = config.krebs.hosts.mors;
          path = "/home/lass/secrets/${config.krebs.build.host.name}";
        };
        dir.stockholm = {
          host = config.krebs.hosts.mors;
          path = "/home/lass/stockholm";
        };
      };
    };
  };

  nix.useChroot = true;

  users.mutableUsers = false;

  #why is this on in the first place?
  services.ntp.enable = false;
  services.nscd.enable = false;

  boot.tmpOnTmpfs = true;
  # see tmpfiles.d(5)
  systemd.tmpfiles.rules = [
    "d /tmp 1777 root root - -"
  ];

  # multiple-definition-problem when defining environment.variables.EDITOR
  environment.extraInit = ''
    EDITOR=vim
    PAGER=most
  '';

  environment.systemPackages = with pkgs; [
  #stockholm
    git
    jq
    parallel

  #style
    most
    rxvt_unicode.terminfo

  #network
    iptables

  #stuff for dl
    aria2
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

  security.setuidPrograms = [
    "sendmail"
  ];

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

}

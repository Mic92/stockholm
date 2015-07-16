{ config, lib, pkgs, ... }:

with lib;
{
  imports = [
    ./sshkeys.nix
    ../../3modules/lass/iptables.nix
    {
      users.extraUsers =
        mapAttrs (_: h: { hashedPassword = h; })
                 (import /root/src/secrets/hashedPasswords.nix);
    }

  ];

  nix.useChroot = true;

  users.mutableUsers = false;

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
    git
    most
    rxvt_unicode.terminfo

  #network
    iptables
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

  services.gitolite = {
    enable = true;
    dataDir = "/home/gitolite";
    adminPubkey = config.sshKeys.lass.pub;
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

  lass.iptables = {
    enable = true;
    tables = {
      filter.INPUT.policy = "DROP";
      filter.FORWARD.policy = "DROP";
      filter.INPUT.rules = [
        { predicate = "-i lo"; target = "ACCEPT"; }
        { predicate = "-m conntrack --ctstate RELATED,ESTABLISHED"; target = "ACCEPT"; }
        { predicate = "-p icmp"; target = "ACCEPT"; }
        { predicate = "-p tcp --dport 22"; target = "ACCEPT"; }
      ];
    };
  };

  #Networking.firewall = {
  #  enable = true;

  #  allowedTCPPorts = [
  #    22
  #  ];

  #  extraCommands = ''
  #    iptables -A INPUT -j ACCEPT -m conntrack --ctstate RELATED,ESTABLISHED
  #    iptables -A INPUT -j ACCEPT -i lo
  #    #http://serverfault.com/questions/84963/why-not-block-icmp
  #    iptables -A INPUT -j ACCEPT -p icmp

  #    #TODO: fix Retiolum firewall
  #    #iptables -N RETIOLUM
  #    #iptables -A INPUT -j RETIOLUM -i retiolum
  #    #iptables -A RETIOLUM -j ACCEPT -m conntrack --ctstate RELATED,ESTABLISHED
  #    #iptables -A RETIOLUM -j REJECT -p tcp --reject-with tcp-reset
  #    #iptables -A RETIOLUM -j REJECT -p udp --reject-with icmp-port-unreachable
  #    #iptables -A RETIOLUM -j REJECT        --reject-with icmp-proto-unreachable
  #    #iptables -A RETIOLUM -j REJECT
  #  '';
  #};
}

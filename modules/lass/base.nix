{ config, pkgs, ... }:

{
  imports = [
    ./sshkeys.nix
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

  networking.firewall = {
    enable = true;

    allowedTCPPorts = [
      22
    ];

    extraCommands = ''
      iptables -A INPUT -j ACCEPT -m conntrack --ctstate RELATED,ESTABLISHED
      iptables -A INPUT -j ACCEPT -i lo

      #iptables -N Retiolum
      iptables -A INPUT -j Retiolum -i retiolum
      iptables -A Retiolum -j ACCEPT -p icmp
      iptables -A Retiolum -j ACCEPT -m conntrack --ctstate RELATED,ESTABLISHED
      iptables -A Retiolum -j REJECT -p tcp --reject-with tcp-reset
      iptables -A Retiolum -j REJECT -p udp --reject-with icmp-port-unreachable
      iptables -A Retiolum -j REJECT        --reject-with icmp-proto-unreachable
      iptables -A Retiolum -j REJECT
    '';

    extraStopCommands = "iptables -F";
  };
}

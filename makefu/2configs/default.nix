{ config, lib, pkgs, ... }:

with lib;
{
  system.stateVersion = "15.09";

  imports = [
    {
      users.extraUsers =
        mapAttrs (_: h: { hashedPassword = h; })
                 (import <secrets/hashedPasswords.nix>);
    }
    ./vim.nix
  ];


  krebs = {
    enable = true;
    search-domain = "retiolum";
    build =  {
      target = mkDefault "root@${config.krebs.build.host.name}";
      user = config.krebs.users.makefu;
      source = {
        git.nixpkgs = {
          #url = https://github.com/NixOS/nixpkgs;
          url = mkDefault https://github.com/makefu/nixpkgs;
          rev = mkDefault "78340b042463fd35caa587b0db2e400e5666dbe1"; # nixos-15.09 + cherry-picking
          target-path = "/var/src/nixpkgs";
        };

        dir.secrets = {
          host = config.krebs.hosts.pornocauster;
          path = "/home/makefu/secrets/${config.krebs.build.host.name}/";
        };

        dir.stockholm = {
          host = config.krebs.hosts.pornocauster;
          path = "/home/makefu/stockholm" ;
          target-path = "/var/src/stockholm";
        };
      };
    };
  };

  users.extraUsers = {
    root = {
        openssh.authorizedKeys.keys = [ config.krebs.users.makefu.pubkey ];
    };
    makefu = {
      uid = 9001;
      group = "users";
      home = "/home/makefu";
      createHome = true;
      useDefaultShell = true;
      extraGroups = [
        "wheel"
      ];
        openssh.authorizedKeys.keys = [ config.krebs.users.makefu.pubkey ];
    };
  };

  networking.hostName = config.krebs.build.host.name;
  nix.maxJobs = config.krebs.build.host.cores;

  time.timeZone = "Europe/Berlin";
  #nix.maxJobs = 1;

  programs.ssh.startAgent = false;
  services.openssh.enable = true;
  nix.useChroot = true;

  users.mutableUsers = false;

  boot.tmpOnTmpfs = true;

  networking.firewall.rejectPackets = true;
  networking.firewall.allowPing = true;

  systemd.tmpfiles.rules = [
    "d /tmp 1777 root root - -"
  ];

  environment.variables.EDITOR = mkForce "vim";

  environment.systemPackages = with pkgs; [
      jq
      git
      gnumake
      rxvt_unicode.terminfo
      htop
  ];

  programs.bash = {
    enableCompletion = true;
    interactiveShellInit = ''
      HISTCONTROL='erasedups:ignorespace'
      HISTSIZE=900001
      HISTFILESIZE=$HISTSIZE

      shopt -s checkhash
      shopt -s histappend histreedit histverify
      shopt -s no_empty_cmd_completion
      '';

    promptInit = ''
      case $UID in
         0) PS1='\[\e[1;31m\]\w\[\e[0m\] ' ;;
      9001) PS1='\[\e[1;32m\]\w\[\e[0m\] ' ;;
         *) PS1='\[\e[1;35m\]\u \[\e[1;32m\]\w\[\e[0m\] ' ;;
      esac
      if test -n "$SSH_CLIENT"; then
        PS1='\[\033[35m\]\h'" $PS1"
      fi
      '';
  };

  environment.shellAliases = {
    lsl = "ls -lAtr";
  };

  nixpkgs.config.packageOverrides = pkgs: {
    nano = pkgs.runCommand "empty" {} "mkdir -p $out";
  };

  services.cron.enable = false;
  services.nscd.enable = false;

  security.setuidPrograms = [ "sendmail" ];
  services.journald.extraConfig = ''
    SystemMaxUse=1G
    RuntimeMaxUse=128M
    '';
  # Enable IPv6 Privacy Extensions
  boot.kernel.sysctl = {
    "net.ipv6.conf.all.use_tempaddr" = 2;
    "net.ipv6.conf.default.use_tempaddr" = 2;
  };

  i18n = {
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };
}

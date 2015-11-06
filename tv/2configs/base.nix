{ config, lib, pkgs, ... }:

with builtins;
with lib;

{
  krebs.enable = true;

  networking.hostName = config.krebs.build.host.name;

  imports = [
    ./vim.nix
    {
      # stockholm dependencies
      environment.systemPackages = with pkgs; [
        git
      ];
    }
    {
      # TODO never put hashedPassword into the store
      users.extraUsers =
        mapAttrs (_: h: { hashedPassword = h; })
                 (import <secrets/hashedPasswords.nix>);
    }
    {
      users.defaultUserShell = "/run/current-system/sw/bin/bash";
      users.mutableUsers = false;
    }
    {
      users.extraUsers = {
        root = {
          openssh.authorizedKeys.keys = [
            config.krebs.users.tv.pubkey
          ];
        };
        tv = {
          uid = 1337;
          group = "users";
          home = "/home/tv";
          createHome = true;
          useDefaultShell = true;
          extraGroups = [
            "audio"
            "video"
            "wheel"
          ];
          openssh.authorizedKeys.keys = [
            config.krebs.users.tv.pubkey
          ];
        };
      };
    }
    {
      security.sudo.extraConfig = ''
        Defaults mailto="${config.krebs.users.tv.mail}"
      '';
      time.timeZone = "Europe/Berlin";
    }
    {
      # TODO check if both are required:
      nix.chrootDirs = [ "/etc/protocols" pkgs.iana_etc.outPath ];

      nix.trustedBinaryCaches = [
        "https://cache.nixos.org"
        "http://cache.nixos.org"
        "http://hydra.nixos.org"
      ];

      nix.useChroot = true;
    }
    {
      environment.systemPackages = with pkgs; [
        rxvt_unicode.terminfo
      ];

      environment.shellAliases = mkForce {
        # alias cal='cal -m3'
        gp = "${pkgs.pari}/bin/gp -q";
        df = "df -h";
        du = "du -h";
        # alias grep='grep --color=auto'

        # TODO alias cannot contain #\'
        # "ps?" = "ps ax | head -n 1;ps ax | fgrep -v ' grep --color=auto ' | grep";

        # alias la='ls -lA'
        lAtr = "ls -lAtr";
        # alias ll='ls -l'
        ls = "ls -h --color=auto --group-directories-first";
        dmesg = "dmesg -L --reltime";
        view = "vim -R";
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

          ${readFile ./bash_completion.sh}

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
    }

    {
      services.cron.enable = false;
      services.nscd.enable = false;
      services.ntp.enable = false;
    }

    {
      boot.kernel.sysctl = {
        # Enable IPv6 Privacy Extensions
        "net.ipv6.conf.all.use_tempaddr" = 2;
        "net.ipv6.conf.default.use_tempaddr" = 2;
      };
    }

    {
      services.openssh = {
        enable = true;
        hostKeys = [
          { type = "ed25519"; path = "/etc/ssh/ssh_host_ed25519_key"; }
        ];
      };
    }

    {
      # TODO: exim
      security.setuidPrograms = [
        "sendmail"  # for sudo
      ];
    }
  ];
}

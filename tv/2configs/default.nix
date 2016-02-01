{ config, lib, pkgs, ... }:

with lib;

{
  krebs.enable = true;

  krebs.build = {
    user = config.krebs.users.tv;
    target = mkDefault "root@${config.krebs.build.host.name}";
    source-version = 2;
    source = mapAttrs (_: mkDefault) ({
      nixos-config = "symlink:stockholm-private/1systems/${config.krebs.build.host.name}.nix";
      nixpkgs = symlink:stockholm-nixpkgs;
      secrets = "/home/tv/secrets/${config.krebs.build.host.name}";
      secrets-common = "/home/tv/secrets/common";
      stockholm-krebs = "/home/tv/stockholm/krebs";
      stockholm-nixpkgs = "/home/tv/stockholm/nixpkgs";
      stockholm-private = "/home/tv/stockholm/tv";
      upstream-nixpkgs = {
        url = https://github.com/NixOS/nixpkgs;
        rev = "77f8f35d57618c1ba456d968524f2fb2c3448295";
        dev = "/home/tv/nixpkgs";
      };
    } // optionalAttrs config.krebs.build.host.secure {
      secrets-master = "/home/tv/secrets/master";
    });
  };

  networking.hostName = config.krebs.build.host.name;

  imports = [
    <secrets>
    ./backup.nix
    ./vim.nix
    {
      # stockholm dependencies
      environment.systemPackages = with pkgs; [
        git
      ];
    }
    {
      users = {
        defaultUserShell = "/run/current-system/sw/bin/bash";
        mutableUsers = false;
        users = {
          tv = {
            isNormalUser = true;
            uid = 1337;
          };
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
      environment.profileRelativeEnvVars.PATH = mkForce [ "/bin" ];

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

        reload = "systemctl reload";
        restart = "systemctl restart";
        start = "systemctl start";
        status = "systemctl status";
        stop = "systemctl stop";
      };

      environment.variables = {
        NIX_PATH = mkForce "/var/src";
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

      programs.ssh = {
        extraConfig = ''
          UseRoaming no
        '';
        startAgent = false;
      };
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
    {
      systemd.tmpfiles.rules = let
        forUsers = flip map users;
        isUser = { name, group, ... }:
          name == "root" || hasSuffix "users" group;
        users = filter isUser (mapAttrsToList (_: id) config.users.users);
      in forUsers (u: "d /run/xdg/${u.name} 0700 ${u.name} ${u.group} -");
      environment.variables.XDG_RUNTIME_DIR = "/run/xdg/$LOGNAME";
    }
  ];
}

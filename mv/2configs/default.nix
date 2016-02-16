{ config, lib, pkgs, ... }:

with config.krebs.lib;
let
  HOME = getEnv "HOME";
in

{
  krebs.enable = true;

  krebs.build = {
    user = config.krebs.users.mv;
    target = mkDefault "root@${config.krebs.build.host.name}";
    source = {
      git.nixpkgs = {
        url = mkDefault https://github.com/NixOS/nixpkgs;
        rev = mkDefault "c44a593aa43bba6a0708f6f36065a514a5110613";
        target-path = mkDefault "/var/src/nixpkgs";
      };
      dir.secrets = {
        path = mkDefault "${HOME}/secrets/${config.krebs.build.host.name}";
      };
      dir.stockholm = {
        path = mkDefault "${HOME}/stockholm";
        target-path = mkDefault "/var/src/stockholm";
      };
    };
  };

  networking.hostName = config.krebs.build.host.name;

  imports = [
    <secrets>
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
          mv = {
            isNormalUser = true;
            uid = 1338;
          };
        };
      };
    }
    {
      security.sudo.extraConfig = ''
        Defaults mailto="${config.krebs.users.mv.mail}"
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
        NIX_PATH =
          with config.krebs.build.source; with dir; with git;
          mkForce (concatStringsSep ":" [
            "nixpkgs=${nixpkgs.target-path}"
            "secrets=${stockholm.target-path}/null"
          ]);
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
    {
      environment.systemPackages = [
        pkgs.get
        pkgs.krebszones
        pkgs.nix-prefetch-scripts
        pkgs.push
      ];
    }

    {
      systemd.tmpfiles.rules = let
        forUsers = flip map users;
        isUser = { group, ... }: hasSuffix "users" group;
        users = filter isUser (mapAttrsToList (_: id) config.users.users);
      in forUsers (u: "d /run/xdg/${u.name} 0700 ${u.name} ${u.group} -");
      environment.variables.XDG_RUNTIME_DIR = "/run/xdg/$LOGNAME";
    }
  ];
}

with import <stockholm/lib>;
{ config, lib, pkgs, ... }: let
  builder = if getEnv "dummy_secrets" == "true"
              then "buildbot"
              else "tv";
in {
  krebs.enable = true;

  krebs.build = {
    user = config.krebs.users.tv;
    source = let inherit (config.krebs.build) host; in {
      nixos-config.symlink = "stockholm/tv/1systems/${host.name}.nix";
      secrets.file = getAttr builder {
        buildbot = toString <stockholm/tv/dummy_secrets>;
        tv = "/home/tv/secrets/${host.name}";
      };
      secrets-common.file = "/home/tv/secrets/common";
      nixpkgs.git = {
        url = https://github.com/NixOS/nixpkgs;
        ref = "412b0a17aa2975e092c7ab95a38561c5f82908d4"; # nixos-17.03
      };
    } // optionalAttrs host.secure {
      secrets-master.file = "/home/tv/secrets/master";
    };
  };

  networking.hostName = config.krebs.build.host.name;

  imports = [
    <secrets>
    ./audit.nix
    ./backup.nix
    ./bash
    ./htop.nix
    ./nginx
    ./ssh.nix
    ./sshd.nix
    ./vim.nix
    ./xdg.nix
    {
      # stockholm dependencies
      environment.systemPackages = with pkgs; [
        git
        gnumake
        hashPassword
        populate
        whatsupnix
      ];
    }
    {
      users = {
        defaultUserShell = "/run/current-system/sw/bin/bash";
        mutableUsers = false;
        users = {
          tv = {
            inherit (config.krebs.users.tv) home uid;
            isNormalUser = true;
            extraGroups = [ "tv" ];
          };
        };
      };
    }
    {
      security.hideProcessInformation = true;
      security.sudo.extraConfig = ''
        Defaults env_keep+="SSH_CLIENT"
        Defaults mailto="${config.krebs.users.tv.mail}"
        Defaults !lecture
      '';
      time.timeZone = "Europe/Berlin";
    }

    {
      # TODO check if both are required:
      nix.sandboxPaths = [ "/etc/protocols" pkgs.iana_etc.outPath ];

      nix.requireSignedBinaryCaches = true;

      nix.binaryCaches = ["https://cache.nixos.org"];

      nix.useSandbox = true;
    }
    {
      nixpkgs.config.allowUnfree = false;
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
        NIX_PATH = mkForce "secrets=/var/src/stockholm/null:/var/src";
      };
    }

    {
      environment.variables =
        flip genAttrs (_: "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt") [
          "CURL_CA_BUNDLE"
          "GIT_SSL_CAINFO"
          "SSL_CERT_FILE"
        ];
    }

    {
      services.cron.enable = false;
      services.nscd.enable = false;
      services.ntp.enable = false;
      services.timesyncd.enable = true;
    }

    {
      boot.kernel.sysctl = {
        # Enable IPv6 Privacy Extensions
        "net.ipv6.conf.all.use_tempaddr" = 2;
        "net.ipv6.conf.default.use_tempaddr" = 2;
      };
    }

    {
      tv.iptables.enable = true;
      tv.iptables.accept-echo-request = "internet";
    }

    {
      services.journald.extraConfig = ''
        SystemMaxUse=1G
        RuntimeMaxUse=128M
      '';
    }

    {
      environment.systemPackages = [
        pkgs.get
        pkgs.htop
        pkgs.krebspaste
        pkgs.nix-prefetch-scripts
        pkgs.ovh-zone
        pkgs.push
      ];
    }
  ];
}

{ config, lib, pkgs, ... }:

{
  imports =
    [
      <secrets/hashedPasswords.nix>
      ./iptables.nix
      ./networking.nix
      ../common/nixpkgs.nix
      ../tv/base.nix
      ../tv/base-cac-CentOS-7-64bit.nix
      ../tv/ejabberd.nix # XXX echtes modul
      ../tv/exim-smarthost.nix
      ../tv/git
      ../tv/retiolum.nix
      ../tv/sanitize.nix
    ];

  # "Developer 2" plan has two vCPUs.
  nix.maxJobs = 2;

  nixpkgs = {
    url = "https://github.com/NixOS/nixpkgs";
    rev = "4c01e6d91993b6de128795f4fbdd25f6227fb870";
  };

  environment.systemPackages = with pkgs; [
    git # required for ./deploy, clone_or_update
    htop
    iftop
    iotop
    iptables
    mutt    # for mv
    nethogs
    rxvt_unicode.terminfo
    tcpdump
  ];

  security.rtkit.enable = false;

  services.cron.enable = false;

  services.ejabberd-cd = {
    enable = true;
  };

  services.git =
    let
      inherit (builtins) readFile;
      # TODO lib should already include our stuff
      inherit (import ../../lib { inherit lib pkgs; }) addNames git;
    in
    rec {
      enable = true;

      users = addNames {
        tv = { pubkey = readFile <pubkeys/tv.ssh.pub>; };
        lass = { pubkey = "xxx"; };
        makefu = { pubkey = "xxx"; };
      };

      repos = addNames {
        shitment = {
          desc = "shitment repository";
          hooks = {
            post-receive = git.irc-announce {
              nick = config.networking.hostName; # TODO make this the default
              channel = "#retiolum";
              server = "ire.retiolum";
            };
          };
          public = true;
        };
        testing = {
          desc = "testing repository";
          hooks = {
            post-receive = git.irc-announce {
              nick = config.networking.hostName; # TODO make this the default
              channel = "#retiolum";
              server = "ire.retiolum";
            };
          };
          public = true;
        };
      };

      rules = with git; with users; with repos; [
        { user = tv;
          repo = [ testing shitment ];
          perm = push "refs/*" [ non-fast-forward create delete merge ];
        }
        { user = [ lass makefu ];
          repo = [ testing shitment ];
          perm = fetch;
        }
      ];
    };

  services.journald.extraConfig = ''
    SystemMaxUse=1G
    RuntimeMaxUse=128M
  '';

  services.ntp.enable = false;

  services.openssh = {
    enable = true;
    hostKeys = [
      # XXX bits here make no science
      { bits = 8192; type = "ed25519"; path = "/etc/ssh/ssh_host_ed25519_key"; }
    ];
    permitRootLogin = "yes";
  };

  services.retiolum = {
    enable = true;
    hosts = <retiolum-hosts>;
    privateKeyFile = "/etc/nixos/secrets/cd.retiolum.rsa_key.priv";
    connectTo = [
      "fastpoke"
      "pigstarter"
      "ire"
    ];
  };

  sound.enable = false;

  # TODO replace by ./modules/cd-users.nix
  users.extraGroups = {

    # ● systemd-tmpfiles-setup.service - Create Volatile Files and Directories
    #    Loaded: loaded (/nix/store/2l33gg7nmncqkpysq9f5fxyhlw6ncm2j-systemd-217/example/systemd/system/systemd-tmpfiles-setup.service)
    #    Active: failed (Result: exit-code) since Mon 2015-03-16 10:29:18 UTC; 4s ago
    #      Docs: man:tmpfiles.d(5)
    #            man:systemd-tmpfiles(8)
    #   Process: 19272 ExecStart=/nix/store/2l33gg7nmncqkpysq9f5fxyhlw6ncm2j-systemd-217/bin/systemd-tmpfiles --create --remove --boot --exclude-prefix=/dev (code=exited, status=1/FAILURE)
    #  Main PID: 19272 (code=exited, status=1/FAILURE)
    # 
    # Mar 16 10:29:17 cd systemd-tmpfiles[19272]: [/usr/lib/tmpfiles.d/legacy.conf:26] Unknown group 'lock'.
    # Mar 16 10:29:18 cd systemd-tmpfiles[19272]: Two or more conflicting lines for /var/log/journal configured, ignoring.
    # Mar 16 10:29:18 cd systemd-tmpfiles[19272]: Two or more conflicting lines for /var/log/journal/7b35116927d74ea58785e00b47ac0f0d configured, ignoring.
    # Mar 16 10:29:18 cd systemd[1]: systemd-tmpfiles-setup.service: main process exited, code=exited, status=1/FAILURE
    # Mar 16 10:29:18 cd systemd[1]: Failed to start Create Volatile Files and Directories.
    # Mar 16 10:29:18 cd systemd[1]: Unit systemd-tmpfiles-setup.service entered failed state.
    # Mar 16 10:29:18 cd systemd[1]: systemd-tmpfiles-setup.service failed.
    # warning: error(s) occured while switching to the new configuration
    lock.gid = 10001;

  };
  users.extraUsers =
    {
      root = {
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEieAihh+o208aeCA14fAtjzyZN/nrpOJt2vZ5VYZp69 deploy@wu"
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAAEAQDFR//RnCvEZAt0F6ExDsatKZ/DDdifanuSL360mqOhaFieKI34RoOwfQT9T+Ga52Vh5V2La6esvlph686EdgzeKLvDoxEwFM9ZYFBcMrNzu4bMTlgE7YUYw5JiORyXNfznBGnme6qpuvx9ibYhUyiZo99kM8ys5YrUHrP2JXQJMezDFZHxT4GFMOuSdh/1daGoKKD6hYL/jEHX8CI4E3BSmKK6ygYr1fVX0K0Tv77lIi5mLXucjR7CytWYWYnhM6DC3Hxpv2zRkPgf3k0x/Y1hrw3V/r0Me5h90pd2C8pFaWA2ZoUT/fmyVqvx1tZPYToU/O2dMItY0zgx2kR0yD+6g7Aahz3R+KlXkV8k5c8bbTbfGnZWDR1ZlbLRM9Yt5vosfwapUD90MmVkpmR3wUkO2sUKi80QfC7b4KvSDXQ+MImbGxMaU5Bnsq1PqLN95q+uat3nlAVBAELkcx51FlE9CaIS65y4J7FEDg8BE5JeuCNshh62VSYRXVSFt8bk3f/TFGgzC8OIo14BhVmiRQQ503Z1sROyf5xLX2a/EJavMm1i2Bs2TH6ROKY9z5Pz8hT5US0r381V8oG7TZyLF9HTtoy3wCYsgWA5EmLanjAsVU2YEeAA0rxzdtYP8Y2okFiJ6u+M4HQZ3Wg3peSodyp3vxdYce2vk4EKeqEFuuS82850DYb7Et7fmp+wQQUT8Q/bMO0DreWjHoMM5lE4LJ4ME6AxksmMiFtfo/4Fe2q9D+LAqZ+ANOcv9M+8Rn6ngiYmuRNd0l/a02q1PEvO6vTfXgcl4f7Z1IULHPEaDNZHCJS1K5RXYFqYQ6OHsTmOm7hnwaRAS97+VFMo1i5uvTx9nYaAcY7yzq3Ckfb67dMBKApGOpJpkvPgfrP7bgBO5rOZXM1opXqVPb09nljAhhAhyCTh1e/8+mJrBo0cLQ/LupQzVxGDgm3awSMPxsZAN45PSWz76zzxdDa1MMo51do+VJHfs7Wl0NcXAQrniOBYL9Wqt0qNkn1gY5smkkISGeQ/vxNap4MmzeZE7b5fpOy+2fpcRVQLpc4nooQzJvSVTFz+25lgZ6iHf45K87gQFMIAri1Pf/EDDpL87az+bRWvWi+BA2kMe1kf+Ay1LyMz8r+g51H0ma0bNFh6+fbWMfUiD9JCepIObclnUJ4NlWfcgHxTf17d/4tl6z4DTcLpCCk8Da77JouSHgvtcRbRlFV1OfhWZLXUsrlfpaQTiItv6TGIr3k7+7b66o3Qw/GQVs5GmYifaIZIz8n8my4XjkaMBd0SZfBzzvFjHMq6YUP9+SbjvReqofuoO+5tW1wTYZXitFFBfwuHlXm6w77K5QDBW6olT7pat41/F5eGxLcz tv@wu"
        ];
      };

      mv = rec {
        name = "mv";
        uid = 1338;
        group = "users";
        home = "/home/${name}";
        createHome = true;
        useDefaultShell = true;
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGer9e2+Lew7vnisgBbsFNECEIkpNJgEaqQqgb9inWkQ mv@vod"
        ];
      };

    };

  users.mutableUsers = false;

}

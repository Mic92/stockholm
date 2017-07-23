{ config, pkgs, ... }:

with import <stockholm/lib>;
{
  imports = [
    <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>
    <stockholm/krebs>
    <stockholm/lass/3modules>
    <stockholm/lass/5pkgs>
    <stockholm/lass/2configs/mc.nix>
    <stockholm/lass/2configs/vim.nix>
    {
      # /dev/stderr doesn't work. I don't know why
      # /proc/self doesn't seem to work correctly
      # /dev/pts is empty except for 1 file
      # my life sucks
      nixpkgs.config.packageOverrides = super: {
        irc-announce = super.callPackage <stockholm/krebs/5pkgs/simple/irc-announce> {
          pkgs = pkgs // {
            coreutils = pkgs.symlinkJoin {
              name = "coreutils-hack";
              paths = [
                (pkgs.writeDashBin "tee" ''
                  if test "$1" = /dev/stderr; then
                    while read -r line; do
                      echo "$line"
                      echo "$line" >&2
                    done
                  else
                    ${super.coreutils}/bin/tee "$@"
                  fi
                '')
                pkgs.coreutils
              ];
            };
          };
        };
      };
      boot.kernelParams = [ "copytoram" ];
    }
    {
      krebs.enable = true;
      krebs.build.user = config.krebs.users.lass;
      krebs.build.host = config.krebs.hosts.iso;
    }
    {
      nixpkgs.config.allowUnfree = true;
    }
    {
      users.extraUsers = {
        root = {
          openssh.authorizedKeys.keys = [
            config.krebs.users.lass.pubkey
            config.krebs.users.lass-shodan.pubkey
            config.krebs.users.lass-icarus.pubkey
          ];
        };
      };
    }
    {
      environment.extraInit = ''
        EDITOR=vim
      '';
    }
    {
      environment.systemPackages = with pkgs; [
      #stockholm
        git
        gnumake
        jq
        parallel
        proot
        populate

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
        pciutils
        pop
        psmisc
        q
        rs
        tmux
        untilport
        usbutils

      #unpack stuff
        p7zip
        unzip
        unrar

      #data recovery
        ddrescue
        ntfs3g
        dosfstools
      ];
    }
    {
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
        '';
        promptInit = ''
          if test $UID = 0; then
            PS1='\[\033[1;31m\]\w\[\033[0m\] '
            PROMPT_COMMAND='echo -ne "\033]0;$$ $USER@$PWD\007"'
          elif test $UID = 1337; then
            PS1='\[\033[1;32m\]\w\[\033[0m\] '
            PROMPT_COMMAND='echo -ne "\033]0;$$ $PWD\007"'
          else
            PS1='\[\033[1;33m\]\u@\w\[\033[0m\] '
            PROMPT_COMMAND='echo -ne "\033]0;$$ $USER@$PWD\007"'
          fi
          if test -n "$SSH_CLIENT"; then
            PS1='\[\033[35m\]\h'" $PS1"
            PROMPT_COMMAND='echo -ne "\033]0;$$ $HOSTNAME $USER@$PWD\007"'
          fi
        '';
      };
    }
    {
      services.openssh = {
        enable = true;
        hostKeys = [
          # XXX bits here make no science
          { bits = 8192; type = "ed25519"; path = "/etc/ssh/ssh_host_ed25519_key"; }
        ];
      };
      systemd.services.sshd.wantedBy = mkForce [ "multi-user.target" ];
    }
    {
      networking.firewall = {
        enable = true;
        allowedTCPPorts = [ 22 ];
      };
    }
    {
      krebs.hidden-ssh.enable = true;
    }
    {
      services.xserver = {
        enable = true;
        #videoDrivers = mkForce [ "ati_unfree" ];

        desktopManager.xterm.enable = false;
        desktopManager.default = "none";
        displayManager.lightdm.enable = true;
        displayManager.lightdm.autoLogin = {
          enable = true;
          user = "lass";
        };
        windowManager.default = "xmonad";
        windowManager.session = [{
          name = "xmonad";
          start = ''
            ${pkgs.xorg.xhost}/bin/xhost +LOCAL:
            ${pkgs.xmonad-lass}/bin/xmonad &
            waitPID=$!
          '';
        }];

        layout = "us";
        xkbModel = "evdev";
        xkbVariant = "altgr-intl";
        xkbOptions = "caps:backspace";
      };
    }
  ];
}

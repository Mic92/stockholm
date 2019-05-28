{ config, pkgs, callPackage, ... }: let
  unstable = import <nixpkgs-unstable> { config = { allowUnfree = true; }; };
in {
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      <stockholm/mb>
    ];

  krebs.build.host = config.krebs.hosts.gr33n;

  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.extraModulePackages = with config.boot.kernelPackages; [ wireguard ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];
  fileSystems."/mnt/public" = {
    device = "//192.168.0.4/public";
    fsType = "cifs";
    options = let
      automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
    in [ "${automount_opts},user,rw,username=mb0,iocharset=utf8,credentials=${config.users.users.mb.home}/.smbcredentials" ];
  };

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "de";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Berlin";

  nixpkgs.config.allowUnfree = true;

  nixpkgs.config.packageOverrides = super: {
    openvpn = super.openvpn.override {
      pkcs11Support = true;
      useSystemd = false;
    };
  };

  environment.shellAliases = {
    ll = "ls -alh";
    ls = "ls --color=tty";
  };

  environment.systemPackages = with pkgs; [
     curl
     fish
     git
     htop
     nmap
     ranger
     tcpdump
     tmux
     traceroute
     tree
     vim
     wcalc
     wget
     xz
  ];

  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  sound.enable = false;

  services.openssh.enable = true;
  services.openssh.passwordAuthentication = false;

  networking.wireless.enable = false;
  networking.networkmanager.enable = false;
  krebs.iptables.enable = true;
  networking.enableIPv6 = false;

   programs.fish = {
    enable = true;
    shellInit = ''
      function ssh_agent --description 'launch the ssh-agent and add the id_rsa identity'
          if begin
              set -q SSH_AGENT_PID
              and kill -0 $SSH_AGENT_PID
              and grep -q '^ssh-agent' /proc/$SSH_AGENT_PID/cmdline
          end
              echo "ssh-agent running on pid $SSH_AGENT_PID"
          else
              eval (command ssh-agent -c | sed 's/^setenv/set -Ux/')
          end
          set -l identity $HOME/.ssh/id_rsa
          set -l fingerprint (ssh-keygen -lf $identity | awk '{print $2}')
          ssh-add -l | grep -q $fingerprint
            or ssh-add $identity
      end
    '';
    promptInit = ''
      function fish_prompt --description 'Write out the prompt'
          set -l color_cwd
          set -l suffix
          set -l nix_shell_info (
              if test "$IN_NIX_SHELL" != ""
                 echo -n " <nix-shell>"
              end
          )
          switch "$USER"
              case root toor
                  if set -q fish_color_cwd_root
                      set color_cwd $fish_color_cwd_root
                  else
                      set color_cwd $fish_color_cwd
                  end
                  set suffix '#'
              case '*'
                  set color_cwd $fish_color_cwd
                  set suffix '>'
          end

          echo -n -s "$USER" @ (set_color green) (prompt_hostname) (set_color normal) "$nix_shell_info" ' ' (set_color $color_cwd) (prompt_pwd) (set_color normal) "$suffix "
      end
    '';
  };

  nix.buildCores = 4;
  system.autoUpgrade.enable = false;
  system.autoUpgrade.channel = "https://nixos.org/channels/nixos-19.03";
  system.stateVersion = "19.03";

}

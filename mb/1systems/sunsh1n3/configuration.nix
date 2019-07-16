
{ config, pkgs, ... }: let
  unstable = import <nixpkgs-unstable> { config = { allowUnfree = true; }; };
in {
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
     <stockholm/mb>
    ];

  krebs.build.host = config.krebs.hosts.sunsh1n3;

  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];
  
  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/disk/by-uuid/5354ba31-c7de-4b55-8f86-a2a437dfbb21";
      preLVM = true;
      allowDiscards = true;
    }
  ];

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "de";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Berlin";

  nixpkgs.config.packageOverrides = super : {
   openvpn = super.openvpn.override { pkcs11Support = true; useSystemd = true ; };
  };

  nixpkgs.config.allowUnfree = true;

  fonts = {
    enableCoreFonts = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      anonymousPro
      corefonts
      dejavu_fonts
      envypn-font
      fira
      gentium
      gohufont
      inconsolata
      liberation_ttf
      powerline-fonts
      source-code-pro
      terminus_font
      ttf_bitstream_vera
      ubuntu_font_family
      unifont
      unstable.cherry
      xorg.fontbitstream100dpi
      xorg.fontbitstream75dpi
      xorg.fontbitstreamtype1
    ];
  };

  environment.systemPackages = with pkgs; [
    wget vim git curl fish
    ag
    chromium
    firefox
    gimp
    p7zip
    htop
    mpv
    mpvc
    nmap
    ntfs3g
    keepassx2
    sshfs
    #unstable.skrooge
    skrooge
    unstable.alacritty
    tmux
    tree
    wcalc
    virtmanager
    virt-viewer
    (wine.override { wineBuild = "wineWow"; }) 
    xz    
    zbackup
  ];

  virtualisation.libvirtd.enable = true;
  virtualisation.kvmgt.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;

  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };
  programs.dconf.enable = true;

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.passwordAuthentication = false;

  krebs.iptables.enable = true;
  #networking.wireless.enable = true;  
  networking.networkmanager.enable = true;
  networking.enableIPv6 = false;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;
  nixpkgs.config.pulseaudio = true;

  services.xserver.enable = true;
  services.xserver.layout = "de";
  services.xserver.xkbOptions = "nodeadkeys";
  services.xserver.libinput.enable = true;

  # Enable the KDE Desktop Environment.
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;

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

          echo -n -s "$USER" @ (set_color yellow) (prompt_hostname) (set_color normal) "$nix_shell_info" ' ' (set_color $color_cwd) (prompt_pwd) (set_color normal) "$suffix "
      end
    '';
  };
  
  nix.buildCores = 4;

  system.stateVersion = "19.09";

}

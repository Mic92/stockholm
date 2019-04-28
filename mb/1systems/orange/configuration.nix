{ config, pkgs, callPackage, ... }: let
    unstable = import <nixpkgs-unstable> { config = { allowUnfree = true; }; };
in {
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      <stockholm/mb>
    ];

  krebs.build.host = config.krebs.hosts.orange;

  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.extraModulePackages = with config.boot.kernelPackages; [ wireguard ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/disk/by-uuid/09a36f91-a713-4b82-8b41-4e7a6acc4acf";
      preLVM = true;
      allowDiscards = true;
    }
  ];

  fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];
  fileSystems."/mnt/public" = {
    device = "//192.168.0.4/public";
    fsType = "cifs";
    options = let
      automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
    in [ "${automount_opts},user,rw,username=mb0,iocharset=utf8,credentials=${config.users.users.mb.home}/.smbcredentials" ];
  };


  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "de";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Berlin";

  nixpkgs.config.packageOverrides = super: {
    openvpn = super.openvpn.override { pkcs11Support = true; useSystemd = false; };
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
    adapta-gtk-theme
    aircrackng
    ag
    arandr
    binutils
    chromium
    cifs-utils
    curl
    evince
    exfat
    feh
    file
    firefox
    freetype
    gimp
    git
    gnupg
    graphite2
    hicolor_icon_theme
    htop
    i3lock
    jq
    keepassx2
    kvm
    lxappearance
    man-pages
    moc
    mpv
    mpvc
    mupdf
    ncdu
    nmap
    openvpn
    pass
    p7zip
    powertop
    ranger
    rofi
    sshfs
    tcpdump
    tmux
    traceroute
    tree
    unstable.alacritty
    unstable.ponyc
    unstable.sublime3
    unstable.youtube-dl
    vim
    virt-viewer
    virtmanager
    vulnix
    wcalc
    wget
    xz
  ];

  environment.shellAliases = {
    ll = "ls -alh";
    ls = "ls --color=tty";
  };

  virtualisation.libvirtd.enable = true;
  #virtualisation.kvmgt.enable = true;

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  services.xserver = {
    enable = true;
    layout = "de";
    xkbVariant = "nodeadkeys";
    libinput.enable = true;
    desktopManager = {
      default = "xfce";
      xterm.enable = false;
      xfce = {
        enable = true;
        noDesktop = true;
        enableXfwm = false;
      };
    };
    windowManager.ratpoison.enable = true;
  };

  services.openssh.enable = true;
  #services.openssh.permitRootLogin = "yes";
  services.openssh.passwordAuthentication = false;

  networking.wireless.enable = false;
  networking.networkmanager.enable = false;
  krebs.iptables.enable = true;
  #networking.nameservers = [ "8.8.8.8" "141.1.1.1" ];
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

          echo -n -s "$USER" @ (set_color yellow) (prompt_hostname) (set_color normal) "$nix_shell_info" ' ' (set_color $color_cwd) (prompt_pwd) (set_color normal) "$suffix "
      end
    '';
  };

  nix.maxJobs = 4;
  nix.buildCores = 4;
  system.autoUpgrade.enable = false;
  system.autoUpgrade.channel = "https://nixos.org/channels/nixos-19.03";
  system.stateVersion = "19.03";

}

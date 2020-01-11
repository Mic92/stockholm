{ config, pkgs, ... }:
with import <stockholm/lib>;

let

  wizard = pkgs.writers.writeBash "wizard" ''
    shopt -s extglob

    echo -n '
      welcome to the computer wizard
      first we will check for internet connectivity
      (press enter to continue)
    '
    read -n 1 -s
    if ! ping -c1 lassul.us; then
      echo 'no internet detectio, you will have to provide credentials'
      read -n 1 -s
      nmtui
    fi

    # ping -c1 lassuls.us || ${pkgs.writeDash "nm-dmenu" ''
    #   set -x
    #   export PATH=$PATH:${pkgs.dmenu}/bin:${pkgs.networkmanagerapplet}/bin
    #   exec ${pkgs.networkmanager_dmenu}/bin/networkmanager_dmenu "$@"
    # ''}

    mode=$(echo -n '
    1. help of the wizard
    2. let the wizard watch and help if needed
    3. I will do it alone
    ' | ${pkgs.fzf}/bin/fzf --reverse)
    case "$mode" in
      1*)
        echo 'mode_1' > /tmp/mode
        systemctl start hidden-ssh-announce.service
        tmux new -s help
        ;;
      2*)
        echo 'mode_2' > /tmp/mode
        ;;
      3*)
        echo 'mode_3' > /tmp/mode
        ;;
      *)
        echo 'no mode selected'
        ;;
    esac
  '';

in {
  imports = [
    <stockholm/krebs>
    <stockholm/lass/3modules>
    <stockholm/lass/2configs/vim.nix>
    {
      nixpkgs.config.packageOverrides = import <stockholm/lass/5pkgs> pkgs;
      krebs.enable = true;
      krebs.build.user = config.krebs.users.lass;
      krebs.build.host = {};
    }
    # {
    #   systemd.services.wizard = {
    #     description = "Computer Wizard";
    #     wantedBy = [ "multi-user.target" ];
    #     serviceConfig = {
    #       ExecStart = pkgs.writers.writeDash "wizard" ''
    #         set -efu
    #         cat <<EOF
    #         welcome to the computer wizard
    #         you can choose between the following modes
    #         echo -n '1\n2\n3' | ${pkgs.fzf}/bin/fzf
    #         EOF
    #       '';
    #       StandardInput = "tty";
    #       StandardOutput = "tty";
    #       # TTYPath = "/dev/tty1";
    #       TTYPath = "/dev/ttyS0";
    #       TTYReset = true;
    #       TTYVTDisallocate = true;
    #       Restart = "always";
    #     };
    #   };
    # }
  ];

  networking.hostName = "wizard";
  nixpkgs.config.allowUnfree = true;

  users.extraUsers = {
    root = {
      openssh.authorizedKeys.keys = [
        config.krebs.users.lass.pubkey
        config.krebs.users.lass-mors.pubkey
      ];
    };
  };

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
    dmenu
    hashPassword
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

  environment.extraInit = ''
    EDITOR=vim
  '';

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
      if ! test -e /tmp/mode; then
        ${wizard}
      fi
    '';
  };

  services.openssh.enable = true;
  systemd.services.sshd.wantedBy = mkForce [ "multi-user.target" ];

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 22 ];
  };
  networking.networkmanager.enable = true;
  networking.wireless.enable = mkForce false;

  krebs.hidden-ssh = {
    enable = true;
    channel = "##lassulus-wizard";

  };
  systemd.services.hidden-ssh-announce.wantedBy = mkForce [];
  services.mingetty.autologinUser = "root";

  nixpkgs.config.packageOverrides = super: {
    dmenu = pkgs.writeDashBin "dmenu" ''
      ${pkgs.fzf}/bin/fzf \
        --history=/dev/null \
        --print-query \
        --prompt=\"$PROMPT\"
    '';
  };

  boot.tmpOnTmpfs = true;
}

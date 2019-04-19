with import <stockholm/lib>;
{ config, pkgs, ... }:
{
  imports = [
    {
      users.users = {
        root = {
          openssh.authorizedKeys.keys = [
            config.krebs.users.mb.pubkey
          ];
        };
        mb = {
          name = "mb";
          uid = 1337;
          home = "/home/mb";
          group = "users";
          createHome = true;
          shell = "/run/current-system/sw/bin/fish";
          extraGroups = [
            "audio"
            "video"
            "fuse"
            "wheel"
          ];
          openssh.authorizedKeys.keys = [
            config.krebs.users.mb.pubkey
          ];
        };
      };
    }
    {
      environment.variables = {
        NIX_PATH = mkForce "secrets=/var/src/stockholm/null:/var/src";
      };
    }
    (let ca-bundle = "/etc/ssl/certs/ca-bundle.crt"; in {
      environment.variables = {
        CURL_CA_BUNDLE = ca-bundle;
        GIT_SSL_CAINFO = ca-bundle;
        SSL_CERT_FILE = ca-bundle;
      };
    })
  ];

  networking.hostName = config.krebs.build.host.name;

  krebs = {
    enable = true;
    build.user = config.krebs.users.mb;
  };

  users.mutableUsers = false;

  services.timesyncd.enable = mkForce true;

  systemd.tmpfiles.rules = [
    "d /tmp 1777 root root - -"
  ];

  # multiple-definition-problem when defining environment.variables.EDITOR
  environment.extraInit = ''
    EDITOR=vim
  '';

  nixpkgs.config.allowUnfree = true;

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
    tcpdump

  #stuff for dl
    aria2

  #neat utils
    fish
    file
    kpaste
    krebspaste
    mosh
    pciutils
    psmisc
    tmux
    untilport
    usbutils

  #unpack stuff
    p7zip

    (pkgs.writeDashBin "sshn" ''
      ${pkgs.openssh}/bin/ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no "$@"
    '')
  ];

  services.openssh = {
    enable = true;
    permitRootLogin = "yes";
    passwordAuthentication = false;
    hostKeys = [
      # XXX bits here make no science
      { bits = 8192; type = "ed25519"; path = "/etc/ssh/ssh_host_ed25519_key"; }
    ];
  };

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

  services.journald.extraConfig = ''
    SystemMaxUse=1G
    RuntimeMaxUse=128M
  '';

  krebs.iptables = {
    enable = true;
    tables = {
      nat.PREROUTING.rules = [
        { predicate = "! -i retiolum -p tcp -m tcp --dport 22"; target = "REDIRECT --to-ports 0"; precedence = 100; }
        { predicate = "-p tcp -m tcp --dport 45621"; target = "REDIRECT --to-ports 22"; precedence = 99; }
      ];
      nat.OUTPUT.rules = [
        { predicate = "-o lo -p tcp -m tcp --dport 45621"; target = "REDIRECT --to-ports 22"; precedence = 100; }
      ];
      filter.INPUT.policy = "DROP";
      filter.FORWARD.policy = "DROP";
      filter.INPUT.rules = [
        { predicate = "-i retiolum -p udp --dport 60000:61000"; target = "ACCEPT";}
        { predicate = "-m conntrack --ctstate RELATED,ESTABLISHED"; target = "ACCEPT"; precedence = 10001; }
        { predicate = "-p icmp"; target = "ACCEPT"; precedence = 10000; }
        { predicate = "-p ipv6-icmp"; target = "ACCEPT"; v4 = false;  precedence = 10000; }
        { predicate = "-i lo"; target = "ACCEPT"; precedence = 9999; }
        { predicate = "-p tcp --dport 22"; target = "ACCEPT"; precedence = 9998; }
        { predicate = "-p tcp -i retiolum"; target = "REJECT --reject-with tcp-reset"; precedence = -10000; }
        { predicate = "-p udp -i retiolum"; target = "REJECT --reject-with icmp-port-unreachable"; v6 = false; precedence = -10000; }
        { predicate = "-i retiolum"; target = "REJECT --reject-with icmp-proto-unreachable"; v6 = false; precedence = -10000; }
      ];
    };
  };
}

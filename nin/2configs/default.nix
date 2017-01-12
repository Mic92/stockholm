{ config, lib, pkgs, ... }:

with import <stockholm/lib>;
{
  imports = [
    ../2configs/nixpkgs.nix
    {
      users.extraUsers =
        mapAttrs (_: h: { hashedPassword = h; })
                 (import <secrets/hashedPasswords.nix>);
    }
    {
      users.extraUsers = {
        root = {
          openssh.authorizedKeys.keys = [
            config.krebs.users.nin.pubkey
          ];
        };
        mainUser = {
          name = "nin";
          uid = 1337;
          home = "/home/nin";
          group = "users";
          createHome = true;
          useDefaultShell = true;
          extraGroups = [
            "audio"
            "fuse"
          ];
          openssh.authorizedKeys.keys = [
            config.krebs.users.nin.pubkey
          ];
        };
      };
    }
    {
      environment.variables = {
        NIX_PATH = mkForce "secrets=/var/src/stockholm/null:/var/src";
      };
    }
    (let ca-bundle = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"; in {
      environment.variables = {
        CURL_CA_BUNDLE = ca-bundle;
        GIT_SSL_CAINFO = ca-bundle;
        SSL_CERT_FILE = ca-bundle;
      };
    })
  ];

  networking.hostName = config.krebs.build.host.name;
  nix.maxJobs = config.krebs.build.host.cores;

  krebs = {
    enable = true;
    search-domain = "retiolum";
    build = {
      user = config.krebs.users.nin;
      source = let inherit (config.krebs.build) host; in {
        nixos-config.symlink = "stockholm/nin/1systems/${host.name}.nix";
        secrets.file = "/home/nin/secrets/${host.name}";
        stockholm.file = getEnv "PWD";
      };
    };
  };

  nix.useSandbox = true;

  services.timesyncd.enable = true;

  #why is this on in the first place?
  services.nscd.enable = false;

  boot.tmpOnTmpfs = true;
  # see tmpfiles.d(5)
  systemd.tmpfiles.rules = [
    "d /tmp 1777 root root - -"
  ];

  # multiple-definition-problem when defining environment.variables.EDITOR
  environment.extraInit = ''
    EDITOR=vim
    MANPAGER=most
  '';

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
  #stockholm
    git
    gnumake
    jq
    proot
    populate
    p7zip
    unzip
    unrar
  ];

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
      elif test $UID = 1337; then
        PS1='\[\033[1;32m\]\w\[\033[0m\] '
      else
        PS1='\[\033[1;33m\]\u@\w\[\033[0m\] '
      fi
      if test -n "$SSH_CLIENT"; then
        PS1='\[\033[35m\]\h'" $PS1"
      fi
    '';
  };

  services.openssh = {
    enable = true;
    hostKeys = [
      # XXX bits here make no science
      { bits = 8192; type = "ed25519"; path = "/etc/ssh/ssh_host_ed25519_key"; }
    ];
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
        { predicate = "-m conntrack --ctstate RELATED,ESTABLISHED"; target = "ACCEPT"; precedence = 10001; }
        { predicate = "-p icmp"; target = "ACCEPT"; precedence = 10000; }
        { predicate = "-i lo"; target = "ACCEPT"; precedence = 9999; }
        { predicate = "-p tcp --dport 22"; target = "ACCEPT"; precedence = 9998; }
        { predicate = "-p tcp -i retiolum"; target = "REJECT --reject-with tcp-reset"; precedence = -10000; }
        { predicate = "-p udp -i retiolum"; target = "REJECT --reject-with icmp-port-unreachable"; v6 = false; precedence = -10000; }
        { predicate = "-i retiolum"; target = "REJECT --reject-with icmp-proto-unreachable"; v6 = false; precedence = -10000; }
      ];
    };
  };

  networking.dhcpcd.extraConfig = ''
    noipv4ll
  '';
}

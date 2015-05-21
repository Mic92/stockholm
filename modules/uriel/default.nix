{ config, pkgs, ... }:

{
  imports = [
    ../lass/desktop-base.nix
    ../lass/retiolum-uriel.nix
    ../lass/xserver-lass.nix
    ../lass/browsers.nix
    ../lass/programs.nix
    ../lass/games.nix
    ../tv/exim-retiolum.nix
    ../lass/pass.nix
    ../lass/vim.nix
    ../lass/urxvt.nix
    ../common/nixpkgs.nix
    ../../secrets/uriel-pw.nix
    ../lass/sshkeys.nix
    ../lass/bird.nix
  ];
  nixpkgs = {
    url = "https://github.com/Lassulus/nixpkgs";
    rev = "b3531eebf625e388d2fa33d56646180236263e74";
  };

  services.gitolite = {
    keys = {
      uriel = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM1v/N0G7k48thX1vIALTdqrdYUvYM+SvHRq/rCcKLC2 lass@mors";
      lass = config.sshKeys.lass.pub;
    };
    config = ''
      repo emse-hsdb
          RW+     =   lass
          R       =   tv
          option hook.post-receive = irc-announce

      repo pong
          RW+     =   lass
          R       =   tv
          option hook.post-receive = irc-announce

      repo load-env
          RW+     =   lass
          RW+     =   uriel
          R       =   tv
          option hook.post-receive = irc-announce

      repo pass
          RW+     =   lass
          RW+     =   uriel

      repo testing
          RW+     =   @all

      repo painload
          RW+     =   lass
          R       =   tv
          R       =   makefu

      repo brain
          RW+     =   lass
          R       =   tv
          R       =   makefu
          option hook.post-receive = irc-announce

      repo services
          RW+     =   lass
          R       =   tv
          R       =   makefu
          option hook.post-receive = irc-announce

      repo emse-drywall
          RW+     =   lass
          R       =   tv
          R       =   uriel
          option hook.post-receive = irc-announce

      repo emse-db
          RW+     =   lass
          R       =   tv
          option hook.post-receive = irc-announce

      repo config
          RW+     =   lass
          RW+     =   uriel
          R       =   fastpoke
    '';

    rc = ''
      %RC = (
          UMASK                           =>  0077,
          GIT_CONFIG_KEYS                 =>  "",
          LOG_EXTRA                       =>  1,
          ROLES => {
              READERS                     =>  1,
              WRITERS                     =>  1,
          },
          LOCAL_CODE                =>  "$ENV{HOME}/.gitolite",
          ENABLE => [
                  'help',
                  'desc',
                  'info',
                  'perms',
                  'writable',
                  'ssh-authkeys',
                  'git-config',
                  'daemon',
                  'gitweb',
                  'repo-specific-hooks',
          ],
      );
      1;
    '';

    hooks.repoSpecific = {
      irc-announce = ''
        #! /bin/sh
        set -euf

        config_file="$GL_ADMIN_BASE/conf/irc-announce.conf"
        if test -f "$config_file"; then
          . "$config_file"
        fi

        # XXX when changing IRC_CHANNEL or IRC_SERVER/_PORT, don't forget to update
        #     any relevant gitolite LOCAL_CODE!
        # CAVEAT we hope that IRC_NICK is unique
        IRC_NICK="''${IRC_NICK-gl$GL_TID}"
        IRC_CHANNEL="''${IRC_CHANNEL-#retiolum}"
        IRC_SERVER="''${IRC_SERVER-ire.retiolum}"
        IRC_PORT="''${IRC_PORT-6667}"

        # for privmsg_cat below
        export IRC_CHANNEL

        # collect users that are mentioned in the gitolite configuration
        interested_users="$(perl -e '
          do "gl-conf";
          print join(" ", keys%{ $one_repo{$ENV{"GL_REPO"}} });
        ')"

        # CAVEAT beware of real TABs in grep pattern!
        # CAVEAT there will never be more than 42 relevant log entries!
        log="$(tail -n 42 "$GL_LOGFILE" | grep "^[^ ]*  $GL_TID ")"
        update_log="$(echo "$log" | grep "^[^ ]*  $GL_TID update")"

        # (debug output)
        env | sed 's/^/env: /'
        echo "$log" | sed 's/^/log: /'

        # see http://gitolite.com/gitolite/dev-notes.html#lff
        reponame=$(echo "$update_log" | cut -f 4)
        username=$(echo "$update_log" | cut -f 5)
        ref_name=$(echo "$update_log" | cut -f 7 | sed 's|^refs/heads/||')
        old_sha=$(echo "$update_log" | cut -f 8)
        new_sha=$(echo "$update_log" | cut -f 9)

        # check if new branch is created
        if test $old_sha = 0000000000000000000000000000000000000000; then
          # TODO what should we really show?
          old_sha=$new_sha^
        fi

        #
        git_log="$(git log $old_sha..$new_sha --pretty=oneline --abbrev-commit)"
        commit_count=$(echo "$git_log" | wc -l)

        # echo2 and cat2 are used output to both, stdout and stderr
        # This is used to see what we send to the irc server. (debug output)
        echo2() { echo "$*"; echo "$*" >&2; }
        cat2() { tee /dev/stderr; }

        # privmsg_cat transforms stdin to a privmsg
        privmsg_cat() { awk '{ print "PRIVMSG "ENVIRON["IRC_CHANNEL"]" :"$0 }'; }

        # ircin is used to feed the output of netcat back to the "irc client"
        # so we can implement expect-like behavior with sed^_^
        # XXX mkselfdestructingtmpfifo would be nice instead of this cruft
        tmpdir="$(mktemp -d irc-announce_XXXXXXXX)"
        cd "$tmpdir"
        mkfifo ircin
        trap "
          rm ircin
          cd '$OLDPWD'
          rmdir '$tmpdir'
          trap - EXIT INT QUIT
        " EXIT INT QUIT

        #
        #
        #
        {
          echo2 "USER $LOGNAME 0 * :$LOGNAME@$(hostname)"
          echo2 "NICK $IRC_NICK"

          # wait for MODE message
          sed -n '/^:[^ ]* MODE /q'

          echo2 "JOIN $IRC_CHANNEL"

          echo "$interested_users" \
            | tr ' ' '\n' \
            | grep -v "^$GL_USER" \
            | sed 's/$/: poke/' \
            | privmsg_cat \
            | cat2

          printf '[13%s] %s pushed %s new commit%s to 6%s %s\n' \
              "$reponame" \
              "$username" \
              "$commit_count" \
              "$(test $commit_count = 1 || echo s)" \
              "$(hostname)" \
              "$ref_name" \
            | privmsg_cat \
            | cat2

          echo "$git_log" \
            | sed 's/^/14/;s/ / /' \
            | privmsg_cat \
            | cat2

          echo2 "PART $IRC_CHANNEL"

          # wait for PART confirmation
          sed -n '/:'"$IRC_NICK"'![^ ]* PART /q'

          echo2 'QUIT :Gone to have lunch'
        } < ircin \
          | nc "$IRC_SERVER" "$IRC_PORT" | tee -a ircin
      '';
    };
  };


  networking.hostName = "uriel";
  networking.wireless.enable = true;
  nix.maxJobs = 2;

  hardware.enableAllFirmware = true;
  nixpkgs.config.allowUnfree = true;

  boot = {
    #kernelParams = [
    #  "acpi.brightness_switch_enabled=0"
    #];
    #loader.grub.enable = true;
    #loader.grub.version = 2;
    #loader.grub.device = "/dev/sda";

    loader.gummiboot.enable = true;
    loader.gummiboot.timeout = 5;

    initrd.luks.devices = [ { name = "luksroot"; device = "/dev/sda2"; } ];
    initrd.luks.cryptoModules = [ "aes" "sha512" "sha1" "xts" ];
    initrd.availableKernelModules = [ "xhci_hcd" "ehci_pci" "ahci" "usb_storage" ];
    #kernelModules = [ "kvm-intel" "msr" ];
    kernelModules = [ "msr" ];
    extraModprobeConfig = ''
    '';
  };
  fileSystems = {
    "/" = {
      device = "/dev/pool/root";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/sda1";
    };
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="64:27:37:7d:d8:ae", NAME="wl0"
    SUBSYSTEM=="net", ATTR{address}=="f0:de:f1:b8:c8:2e", NAME="et0"
  '';

  #services.xserver = {
  #};

  services.xserver.synaptics = {
    enable = true;
    twoFingerScroll = true;
    accelFactor = "0.035";
    additionalOptions = ''
      Option "FingerHigh" "60"
      Option "FingerLow"  "60"
    '';
  };

  users.extraUsers = {
    root = {
      openssh.authorizedKeys.keys = [
        config.sshKeys.lass.pub
      ];
    };
    mainUser = {
      uid = 1337;
      name = "lass";
      #isNormalUser = true;
      group = "users";
      createHome = true;
      home = "/home/lass";
      useDefaultShell = true;
      isSystemUser = false;
      description = "lassulus";
      extraGroups = [ "wheel" "audio" ];
      openssh.authorizedKeys.keys = [
        config.sshKeys.lass.pub
      ];
    };
  };

  environment.systemPackages = with pkgs; [
  ];


  #users.extraGroups = {
  #  loot = {
  #    members = [
  #      "lass"
  #      "firefox"
  #      "chromium"
  #      "google"
  #    ];
  #  };
  #};
  #
  # iptables
  #
  #networking.firewall.enable = false;
  #system.activationScripts.iptables =
  #  let
  #    log = false;
  #    when = c: f: if c then f else "";
  #  in
  #    ''
  #      ip4tables() { ${pkgs.iptables}/sbin/iptables "$@"; }
  #      ip6tables() { ${pkgs.iptables}/sbin/ip6tables "$@"; }
  #      ipXtables() { ip4tables "$@"; ip6tables "$@"; }

  #      #
  #      # nat
  #      #

  #      # reset tables
  #      ipXtables -t nat -F
  #      ipXtables -t nat -X

  #      #
  #      #ipXtables -t nat -A PREROUTING -j REDIRECT ! -i retiolum -p tcp --dport ssh --to-ports 0
  #      ipXtables -t nat -A PREROUTING -j REDIRECT -p tcp --dport 11423 --to-ports ssh

  #      #
  #      # filter
  #      #

  #      # reset tables
  #      ipXtables -P INPUT DROP
  #      ipXtables -P FORWARD DROP
  #      ipXtables -F
  #      ipXtables -X

  #      # create custom chains
  #      ipXtables -N Retiolum

  #      # INPUT
  #      ipXtables -A INPUT -j ACCEPT -m conntrack --ctstate RELATED,ESTABLISHED
  #      ipXtables -A INPUT -j ACCEPT -i lo
  #      ipXtables -A INPUT -j ACCEPT -p tcp --dport ssh -m conntrack --ctstate NEW
  #      ipXtables -A INPUT -j ACCEPT -p tcp --dport http -m conntrack --ctstate NEW
  #      ipXtables -A INPUT -j ACCEPT -p tcp --dport tinc -m conntrack --ctstate NEW
  #      ipXtables -A INPUT -j Retiolum -i retiolum
  #      ${when log "ipXtables -A INPUT -j LOG --log-level info --log-prefix 'INPUT DROP '"}

  #      # FORWARD
  #      ${when log "ipXtables -A FORWARD -j LOG --log-level info --log-prefix 'FORWARD DROP '"}

  #      # Retiolum
  #      ip4tables -A Retiolum -j ACCEPT -p icmp --icmp-type echo-request
  #      ip6tables -A Retiolum -j ACCEPT -p ipv6-icmp -m icmp6 --icmpv6-type echo-request


  #      ${when log "ipXtables -A Retiolum -j LOG --log-level info --log-prefix 'REJECT '"}
  #      ipXtables -A Retiolum -j REJECT -p tcp --reject-with tcp-reset
  #      ip4tables -A Retiolum -j REJECT -p udp --reject-with icmp-port-unreachable
  #      ip4tables -A Retiolum -j REJECT        --reject-with icmp-proto-unreachable
  #      ip6tables -A Retiolum -j REJECT -p udp --reject-with icmp6-port-unreachable
  #      ip6tables -A Retiolum -j REJECT

  #    '';
}

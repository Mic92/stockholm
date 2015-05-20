{ config, pkgs, ... }:

{
  imports = [
    ./sshkeys.nix
  ];

  nix.useChroot = true;

  users.mutableUsers = false;

  boot.tmpOnTmpfs = true;
  # see tmpfiles.d(5)
  systemd.tmpfiles.rules = [
    "d /tmp 1777 root root - -"
  ];

  # multiple-definition-problem when defining environment.variables.EDITOR
  environment.extraInit = ''
    EDITOR=vim
    PAGER=most
  '';

  environment.systemPackages = with pkgs; [
    git
    most

  #network
    iptables
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

      #fancy colors
      if [ -e ~/LS_COLORS ]; then
        eval $(dircolors ~/LS_COLORS)
      fi

      if [ -e /etc/nixos/dotfiles/link ]; then
        /etc/nixos/dotfiles/link
      fi
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

  services.gitolite = {
    enable = true;
    dataDir = "/home/gitolite";
    adminPubkey = config.sshKeys.lass.pub;
    #commonHooks = [
    #  (pkgs.writeText "irc-announce" ''
    #    #! /bin/sh
    #    set -euf

    #    config_file="$GL_ADMIN_BASE/conf/irc-announce.conf"
    #    if test -f "$config_file"; then
    #      . "$config_file"
    #    fi

    #    # XXX when changing IRC_CHANNEL or IRC_SERVER/_PORT, don't forget to update
    #    #     any relevant gitolite LOCAL_CODE!
    #    # CAVEAT we hope that IRC_NICK is unique
    #    IRC_NICK="''${IRC_NICK-gl$GL_TID}"
    #    IRC_CHANNEL="''${IRC_CHANNEL-#retiolum}"
    #    IRC_SERVER="''${IRC_SERVER-ire.retiolum}"
    #    IRC_PORT="''${IRC_PORT-6667}"

    #    # for privmsg_cat below
    #    export IRC_CHANNEL

    #    # collect users that are mentioned in the gitolite configuration
    #    interested_users="$(perl -e '
    #      do "gl-conf";
    #      print join(" ", keys%{ $one_repo{$ENV{"GL_REPO"}} });
    #    ')"

    #    # CAVEAT beware of real TABs in grep pattern!
    #    # CAVEAT there will never be more than 42 relevant log entries!
    #    log="$(tail -n 42 "$GL_LOGFILE" | grep "^[^ ]*  $GL_TID ")"
    #    update_log="$(echo "$log" | grep "^[^ ]*  $GL_TID update")"

    #    # (debug output)
    #    env | sed 's/^/env: /'
    #    echo "$log" | sed 's/^/log: /'

    #    # see http://gitolite.com/gitolite/dev-notes.html#lff
    #    reponame=$(echo "$update_log" | cut -f 4)
    #    username=$(echo "$update_log" | cut -f 5)
    #    ref_name=$(echo "$update_log" | cut -f 7 | sed 's|^refs/heads/||')
    #    old_sha=$(echo "$update_log" | cut -f 8)
    #    new_sha=$(echo "$update_log" | cut -f 9)

    #    # check if new branch is created
    #    if test $old_sha = 0000000000000000000000000000000000000000; then
    #      # TODO what should we really show?
    #      old_sha=$new_sha^
    #    fi

    #    #
    #    git_log="$(git log $old_sha..$new_sha --pretty=oneline --abbrev-commit)"
    #    commit_count=$(echo "$git_log" | wc -l)

    #    # echo2 and cat2 are used output to both, stdout and stderr
    #    # This is used to see what we send to the irc server. (debug output)
    #    echo2() { echo "$*"; echo "$*" >&2; }
    #    cat2() { tee /dev/stderr; }

    #    # privmsg_cat transforms stdin to a privmsg
    #    privmsg_cat() { awk '{ print "PRIVMSG "ENVIRON["IRC_CHANNEL"]" :"$0 }'; }

    #    # ircin is used to feed the output of netcat back to the "irc client"
    #    # so we can implement expect-like behavior with sed^_^
    #    # XXX mkselfdestructingtmpfifo would be nice instead of this cruft
    #    tmpdir="$(mktemp -d irc-announce_XXXXXXXX)"
    #    cd "$tmpdir"
    #    mkfifo ircin
    #    trap "
    #      rm ircin
    #      cd '$OLDPWD'
    #      rmdir '$tmpdir'
    #      trap - EXIT INT QUIT
    #    " EXIT INT QUIT

    #    #
    #    #
    #    #
    #    {
    #      echo2 "USER $LOGNAME 0 * :$LOGNAME@$(hostname)"
    #      echo2 "NICK $IRC_NICK"

    #      # wait for MODE message
    #      sed -n '/^:[^ ]* MODE /q'

    #      echo2 "JOIN $IRC_CHANNEL"

    #      echo "$interested_users" \
    #        | tr ' ' '\n' \
    #        | grep -v "^$GL_USER" \
    #        | sed 's/$/: poke/' \
    #        | privmsg_cat \
    #        | cat2

    #      printf '[13%s] %s pushed %s new commit%s to 6%s %s\n' \
    #          "$reponame" \
    #          "$username" \
    #          "$commit_count" \
    #          "$(test $commit_count = 1 || echo s)" \
    #          "$(hostname)" \
    #          "$ref_name" \
    #        | privmsg_cat \
    #        | cat2

    #      echo "$git_log" \
    #        | sed 's/^/14/;s/ / /' \
    #        | privmsg_cat \
    #        | cat2

    #      echo2 "PART $IRC_CHANNEL"

    #      # wait for PART confirmation
    #      sed -n '/:'"$IRC_NICK"'![^ ]* PART /q'

    #      echo2 'QUIT :Gone to have lunch'
    #    } < ircin \
    #      | nc "$IRC_SERVER" "$IRC_PORT" | tee -a ircin
    #  '')
    #];
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

  networking.firewall = {
    enable = true;

    allowedTCPPorts = [
      22
    ];

    extraCommands = ''
      iptables -A INPUT -j ACCEPT -m conntrack --ctstate RELATED,ESTABLISHED
      iptables -A INPUT -j ACCEPT -i lo

      #iptables -N Retiolum
      iptables -A INPUT -j Retiolum -i retiolum
      iptables -A Retiolum -j ACCEPT -p icmp
      iptables -A Retiolum -j ACCEPT -m conntrack --ctstate RELATED,ESTABLISHED
      iptables -A Retiolum -j REJECT -p tcp --reject-with tcp-reset
      iptables -A Retiolum -j REJECT -p udp --reject-with icmp-port-unreachable
      iptables -A Retiolum -j REJECT        --reject-with icmp-proto-unreachable
      iptables -A Retiolum -j REJECT
    '';

    extraStopCommands = "iptables -F";
  };
}

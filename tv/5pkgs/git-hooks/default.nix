{ lib, pkgs, ... }:

with lib;

let
  out = {
    inherit irc-announce;
  };

  # TODO irc-announce should return a derivation
  irc-announce = { nick, channel, server, port ? 6667, verbose ? false }: ''
    #! /bin/sh
    set -euf

    export PATH=${makeSearchPath "bin" (with pkgs; [
      coreutils
      git
      gnused
    ])}

    green()  { printf '\x0303,99%s\x0F' "$1"; }
    red()    { printf '\x0304,99%s\x0F' "$1"; }
    orange() { printf '\x0307,99%s\x0F' "$1"; }
    pink()   { printf '\x0313,99%s\x0F' "$1"; }
    gray()   { printf '\x0314,99%s\x0F' "$1"; }

    unset message
    add_message() {
      message="''${message+$message
    }$*"
    }

    nick=${escapeShellArg nick}
    channel=${escapeShellArg channel}
    server=${escapeShellArg server}
    port=${toString port}

    host=$nick
    cgit_endpoint=http://cgit.$host

    empty=0000000000000000000000000000000000000000

    while read oldrev newrev ref; do

      if [ $oldrev = $empty ]; then
        receive_mode=create
      elif [ $newrev = $empty ]; then
        receive_mode=delete
      elif [ "$(git merge-base $oldrev $newrev)" = $oldrev ]; then
        receive_mode=fast-forward
      else
        receive_mode=non-fast-forward
      fi

      h=$(echo $ref | sed 's:^refs/heads/::')

      # empty_tree=$(git hash-object -t tree /dev/null)
      empty_tree=4b825dc6

      id=$(echo $newrev | cut -b-7)
      id2=$(echo $oldrev | cut -b-7)
      if [ $newrev = $empty ]; then id=$empty_tree; fi
      if [ $oldrev = $empty ]; then id2=$empty_tree; fi

      case $receive_mode in
        create)
          link="$cgit_endpoint/$GIT_SSH_REPO/?h=$h"
          ;;
        delete)
          link="$cgit_endpoint/$GIT_SSH_REPO/ ($h)"
          ;;
        fast-forward|non-fast-forward)
          link="$cgit_endpoint/$GIT_SSH_REPO/diff/?h=$h&id=$id&id2=$id2"
          ;;
      esac

      #$host $GIT_SSH_REPO $ref $link
      add_message $(pink push) $link $(gray "($receive_mode)")

      ${optionalString verbose ''
        add_message "$(
          git log \
              --format="$(orange %h) %s $(gray '(%ar)')" \
              --reverse \
              $id2..$id

          git diff --stat $id2..$id \
            | sed '$!s/\(+*\)\(-*\)$/'$(green '\1')$(red '\2')'/'
        )"
      ''}

    done

    if test -n "''${message-}"; then
      exec ${irc-announce-script} \
        "$server" \
        "$port" \
        "$nick" \
        "$channel" \
        "$message"
    fi
  '';

  irc-announce-script = pkgs.writeScript "irc-announce-script" ''
    #! /bin/sh
    set -euf

    export PATH=${makeSearchPath "bin" (with pkgs; [
      coreutils
      gawk
      gnused
      netcat
      nettools
    ])}

    IRC_SERVER=$1
    IRC_PORT=$2
    IRC_NICK=$3$$
    IRC_CHANNEL=$4
    message=$5

    export IRC_CHANNEL # for privmsg_cat

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

    {
      echo2 "USER $LOGNAME 0 * :$LOGNAME@$(hostname)"
      echo2 "NICK $IRC_NICK"

      # wait for MODE message
      sed -n '/^:[^ ]* MODE /q'

      echo2 "JOIN $IRC_CHANNEL"

      printf '%s' "$message" \
        | privmsg_cat \
        | cat2

      echo2 "PART $IRC_CHANNEL"

      # wait for PART confirmation
      sed -n '/:'"$IRC_NICK"'![^ ]* PART /q'

      echo2 'QUIT :Gone to have lunch'
    } < ircin \
      | nc "$IRC_SERVER" "$IRC_PORT" | tee -a ircin
  '';

in out

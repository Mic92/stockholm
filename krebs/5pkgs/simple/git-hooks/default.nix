{ pkgs, stockholm, ... }:

with stockholm.lib;

{
  # TODO irc-announce should return a derivation
  #      but it cannot because krebs.git.repos.*.hooks :: attrsOf str
  irc-announce =
  { cgit_endpoint ? "http://cgit.${nick}.r"
  , channel
  , nick
  , port ? 6667
  , refs ? []
  , server
  , tls ? false
  , verbose ? false
  }: /* sh */ ''
    #! /bin/sh
    set -euf

    export PATH=${makeBinPath (with pkgs; [
      coreutils
      git
      gnugrep
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
    tls=${escapeShellArg tls}
    port=${toString port}

    host=$nick

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

      ${optionalString (refs != []) ''
        if ! { echo "$ref" | grep -qE "${concatStringsSep "|" refs}"; }; then
          echo "we are not announcing this ref: $h"
          exit 0
        fi
      ''}

      h=$(echo $ref | sed 's:^refs/heads/::')

      # empty_tree=$(git hash-object -t tree /dev/null)
      empty_tree=4b825dc6

      id=$(echo $newrev | cut -b-7)
      id2=$(echo $oldrev | cut -b-7)
      if [ $newrev = $empty ]; then id=$empty_tree; fi
      if [ $oldrev = $empty ]; then id2=$empty_tree; fi

      ${if cgit_endpoint != null then /* sh */ ''
        cgit_endpoint=${escapeShellArg cgit_endpoint}
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
      '' else /* sh */ ''
        link="$GIT_SSH_REPO $h"
      ''}

      #$host $GIT_SSH_REPO $ref $link
      add_message $(pink push) $link $(gray "($receive_mode)")

      ${optionalString (verbose == true || typeOf verbose == "set") /* sh */ ''
        ${optionalString (verbose.exclude or [] != []) /* sh */ ''
          case $ref in (${concatStringsSep "|" verbose.exclude})
            continue
          esac
        ''}
        add_message "$(
          git log \
              --format="$(orange %h) %s $(gray '(%ar)')" \
              --no-merges \
              --reverse \
              $id2..$id

          git diff --stat $id2..$id \
            | sed '$!s/\(+*\)\(-*\)$/'$(green '\1')$(red '\2')'/'
        )"
      ''}

    done

    if test -n "''${message-}"; then
      exec ${pkgs.irc-announce}/bin/irc-announce \
        "$server" \
        "$port" \
        "$nick" \
        "$channel" \
        "$tls" \
        "$message"
    fi
  '';
}

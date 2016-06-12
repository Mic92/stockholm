{ pkgs, ... }:
let
  q-cal = let
    # XXX 23 is the longest line of cal's output
    pad = ''{
      ${pkgs.gnused}/bin/sed '
            # rtrim
            s/ *$//

            # delete last empty line
            ''${/^$/d}
          ' \
        | ${pkgs.gawk}/bin/awk '{printf "%-23s\n", $0}' \
        | ${pkgs.gnused}/bin/sed '
              # colorize header
              1,2s/.*/[38;5;238;1m&[39;22m/

              # colorize week number
              s/^[ 1-9][0-9]/[38;5;238;1m&[39;22m/
            '
    }'';
  in ''
    ${pkgs.coreutils}/bin/paste \
        <(${pkgs.utillinux}/bin/cal -mw \
              $(${pkgs.coreutils}/bin/date +'%m %Y' -d 'last month') \
            | ${pad}
        ) \
        <(${pkgs.utillinux}/bin/cal -mw \
            | ${pkgs.gnused}/bin/sed '
                # colorize day of month
                s/\(^\| \)'"$(${pkgs.coreutils}/bin/date +%e)"'\>/[31;1m&[39;22m/
              ' \
            | ${pad}
        ) \
        <(${pkgs.utillinux}/bin/cal -mw \
              $(${pkgs.coreutils}/bin/date +'%m %Y' -d 'next month') \
            | ${pad}
        ) \
      | ${pkgs.gnused}/bin/sed 's/\t/  /g'
  '';

  q-isodate = ''
    ${pkgs.coreutils}/bin/date \
        '+[1m%Y-%m-%d[;30mT[;38;5;085m%H:%M[m:%S%:z'
  '';

  # Singapore's red is #ED2E38
  q-sgtdate = ''
    TZ=Asia/Singapore \
    ${pkgs.coreutils}/bin/date \
        '+[1m%Y-%m-%d[;30mT[;38;5;088m%H:%M[m:%S%:z'
  '';

  q-gitdir = ''
    if test -d .git; then
      #git status --porcelain
      branch=$(
        ${pkgs.git}/bin/git branch \
          | ${pkgs.gnused}/bin/sed -rn 's/^\* (.*)/\1/p'
      )
      echo "± $LOGNAME@''${HOSTNAME-$(${pkgs.nettools}/bin/hostname)}:$PWD .git $branch"
    fi
  '';

  q-power_supply = ''
    for uevent in /sys/class/power_supply/*/uevent; do
      if test -f $uevent; then
        eval "$(${pkgs.gnused}/bin/sed -n '
          s/^\([A-Z_]\+=\)\(.*\)/\1'\'''\2'\'''/p
        ' $uevent)"

        if test "x''${POWER_SUPPLY_CHARGE_NOW-}" = x; then
          continue
        fi

        charge_percentage=$(echo "
          scale=2
          $POWER_SUPPLY_CHARGE_NOW / $POWER_SUPPLY_CHARGE_FULL
        " | ${pkgs.bc}/bin/bc)

        lfc=$POWER_SUPPLY_CHARGE_FULL
        rc=$POWER_SUPPLY_CHARGE_NOW
        #rc=2800
        N=78; N=76
        N=10
        n=$(echo $N-1 | ${pkgs.bc}/bin/bc)
        centi=$(echo "$rc*100/$lfc" | ${pkgs.bc}/bin/bc)
        deci=$(echo "$rc*$N/$lfc" | ${pkgs.bc}/bin/bc)
        energy_evel=$(
          echo -n '☳ ' # TRIGRAM FOR THUNDER
          if   test $centi -ge 42; then echo -n '[1;32m'
          elif test $centi -ge 23; then echo -n '[1;33m'
          elif test $centi -ge 11; then echo -n '[1;31m'
          else                        echo -n '[5;1;31m'; fi
          for i in $(${pkgs.coreutils}/bin/seq 1 $deci); do
            echo -n ■
          done
          echo -n '[;30m'
          for i in $(${pkgs.coreutils}/bin/seq $deci $n); do
            echo -n ■
          done
          echo '[m' $rc #/ $lfc
        )
        echo "$energy_evel $charge_percentage"
      fi
    done
  '';

  q-virtualization = ''
    echo "VT: $(${pkgs.systemd}/bin/systemd-detect-virt)"
  '';

  q-wireless = ''
    for dev in $(
      ${pkgs.iw}/bin/iw dev \
        | ${pkgs.gnused}/bin/sed -n 's/^\s*Interface\s\+\([0-9a-z]\+\)$/\1/p'
    ); do
      inet=$(${pkgs.iproute}/bin/ip addr show $dev \
        | ${pkgs.gnused}/bin/sed -n '
            s/.*inet \([0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+\).*/\1/p
          ') \
        || unset inet
      ssid=$(${pkgs.iw}/bin/iw dev $dev link \
        | ${pkgs.gnused}/bin/sed -n '
            s/.*\tSSID: \(.*\)/\1/p
          ') \
        || unset ssid
      echo "$dev''${inet+ $inet}''${ssid+ $ssid}"
    done
  '';

  q-online = ''
    if ${pkgs.curl}/bin/curl -s google.com >/dev/null; then
      echo '[32;1monline[m'
    else
      echo offline
    fi
  '';

  q-thermal_zone = ''
    for i in /sys/class/thermal/thermal_zone*; do
      type=$(${pkgs.coreutils}/bin/cat $i/type)
      temp=$(${pkgs.coreutils}/bin/cat $i/temp)
      printf '%s %s°C\n' $type $(echo $temp / 1000 | ${pkgs.bc}/bin/bc)
    done
  '';

  q-todo = ''
    TODO_file=$HOME/TODO
    if test -e "$TODO_file"; then
      ${pkgs.coreutils}/bin/cat "$TODO_file" \
        | ${pkgs.gawk}/bin/gawk -v now=$(${pkgs.coreutils}/bin/date +%s) '
            BEGIN { print "remind=0" }
            /^[0-9]/{
              x = $1
              gsub(".", "\\\\&", x)
              rest = substr($0, index($0, " "))
              rest = $0
              sub(" *", "", rest)
              gsub(".", "\\\\&", rest)
              print "test $(${pkgs.coreutils}/bin/date +%s -d"x") -lt "now" && \
                echo \"\x1b[38;5;208m\""rest esc "\"\x1b[m\" && \
                (( remind++ ))"
            }
            END { print "test $remind = 0 && echo \"nothing to remind\"" }
          ' \
        | {
          # bash needed for (( ... ))
          ${pkgs.bash}/bin/bash
        }
    else
      echo "$TODO_file: no such file or directory"
    fi
  '';

in
# bash needed for <(...)
pkgs.writeBashBin "q" ''
  set -eu
  export PATH=/var/empty
  ${q-cal}
  echo
  ${q-isodate}
  ${q-sgtdate}
  (${q-gitdir}) &
  (${q-power_supply}) &
  (${q-virtualization}) &
  (${q-wireless}) &
  (${q-online}) &
  (${q-thermal_zone}) &
  wait
  ${q-todo}
''

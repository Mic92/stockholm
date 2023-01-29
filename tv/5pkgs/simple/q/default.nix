with import ./lib;
{ pkgs }:
let
  q-cal = let

    # Maximum width of cal's output.
    calwidth = 23;

    # Number of space characters between two calendars.
    hspace = 2;

    # Return number of columns required to print n calenders side by side.
    need_width = n: assert n >= 1; n * calwidth + (n - 1) * hspace;

  in /* sh */ ''
    cols=$(${pkgs.ncurses}/bin/tput cols)
    if test $cols -ge ${toString (need_width 3)}; then
      ${pkgs.utillinux}/bin/cal --color=always -mw3
    elif test $cols -ge ${toString (need_width 2)}; then
      ${pkgs.utillinux}/bin/cal --color=always -mw -n 2
    elif test $cols -ge ${toString (need_width 1)}; then
      ${pkgs.utillinux}/bin/cal --color=always -mw1
    else
      :
    fi |
    ${pkgs.gnused}/bin/sed -r '
      # dim week numbers
      s/((^ *|  )[ 1-5][0-9](   *)?)(([ 1-3][0-9])*)/[38;5;243m\1[m\4/g
      # dim month and day names
      s/^ *[A-Z].*/[38;5;243m&[m/
      # highlight current date
      s/\[7m/[38;5;009;1m/
      s/\[27m/[m/
    '
  '';

  q-isodate = TZ: color: /* sh */ ''
    TZ=${shell.escape TZ} \
    ${pkgs.coreutils}/bin/date \
        '+[m%Y-%m-%d[38;5;243mT[;'${shell.escape color}'m%H:%M[38;5;243m:[m%S%:z'
  '';

  q-deudate = q-isodate "Europe/Berlin" "38;5;085";

  # Singapore's red is #ED2E38
  q-sgtdate = q-isodate "Asia/Singapore" "38;2;237;46;56";

  q-thadate = q-isodate "Asia/Bangkok" "38;5;226";

  q-utcdate = q-isodate "UTC" "38;5;065";

  q-gitdir = /* sh */ ''
    if test -d .git; then
      #git status --porcelain
      branch=$(
        ${pkgs.git}/bin/git branch \
          | ${pkgs.gnused}/bin/sed -rn 's/^\* (.*)/\1/p'
      )
      echo "± $LOGNAME@''${HOSTNAME-$(${pkgs.nettools}/bin/hostname)}:$PWD .git $branch"
    fi
  '';

  q-intel_backlight = /* sh */ ''
    cd /sys/class/backlight/intel_backlight
    </dev/null exec ${pkgs.gawk}/bin/awk '
      END {
        getline actual_brightness < "actual_brightness"
        getline max_brightness < "max_brightness"
        getline brightness < "brightness"
        printf "intel_backlight %d%% %d/%d\n" \
            , actual_brightness / max_brightness * 100 \
            , actual_brightness \
            , max_brightness
      }
    '
  '';

  q-virtualization = /* sh */ ''
    echo "VT: $(${pkgs.systemd}/bin/systemd-detect-virt)"
  '';

  q-wireless = /* sh */ ''
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

  q-online = /* sh */ ''
    if ${pkgs.curl}/bin/curl -s google.com >/dev/null; then
      echo '[32;1monline[m'
    else
      echo offline
    fi
  '';

  q-thermal_zone = /* sh */ ''
    for i in /sys/class/thermal/thermal_zone*; do
      type=$(${pkgs.coreutils}/bin/cat $i/type)
      temp=$(${pkgs.coreutils}/bin/cat $i/temp)
      printf '%s %s°C\n' $type $(echo $temp / 1000 | ${pkgs.bc}/bin/bc)
    done
  '';

  q-todo = /* sh */ ''
    TODO_file=$PWD/TODO
    if test -e "$TODO_file"; then
      ${pkgs.jq}/bin/jq -Rrs <"$TODO_file" -f ${pkgs.writeJq "q-todo.jq" ''
        split("\n") | map(
          (match("^([0-9]+-\\d{2}-\\d{2})\\s+(.*)$").captures | map(.string))
            as $captures |
          ($captures[0] | strptime("%Y-%m-%d") | mktime) as $date |
          $captures[1] as $text |

          select(now >= $date) |

          ($text | test("\\[URGENT]"; "i")) as $urgent |
          (if $urgent then "38;5;196" else "38;5;208" end) as $sgr |
          if $urgent then sub("\\s*\\[URGENT]\\s*"; " "; "i") else . end |

          "\u001b[\($sgr)m\(.)\u001b[m"
        ) |
        if length == 0 then "nothing to remind" else .[] end
      ''}
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
  ${q-utcdate}
  ${q-deudate}
  ${q-sgtdate}
  (${q-gitdir}) &
  (${q-intel_backlight}) &
  ${pkgs.q-power_supply}/bin/q-power_supply &
  (${q-virtualization}) &
  (${q-wireless}) &
  (${q-online}) &
  (${q-thermal_zone}) &
  wait
  if test "$PWD" != "$HOME" && test -e "$HOME/TODO"; then
    TODO_home_entries=$(cd; (${q-todo}) | ${pkgs.coreutils}/bin/wc -l)
    if test "$TODO_home_entries" = 1; then
      TODO_format='There is %d entry in ~/TODO'
    else
      TODO_format='There are %d entries in ~/TODO'
    fi
    printf "\x1b[38;5;238m$TODO_format\x1b[m\n" "$TODO_home_entries"
  fi
  (${q-todo}) || :
''

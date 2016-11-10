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

  q-intel_backlight = ''
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

  q-power_supply = let
    power_supply = pkgs.writeBash "power_supply" ''
      set -efu
      uevent=$1
      eval "$(${pkgs.gnused}/bin/sed -n '
        s/^\([A-Z_]\+=[0-9A-Za-z_-]*\)$/export \1/p
      ' $uevent)"
      if test "x''${POWER_SUPPLY_CHARGE_NOW-}" = x; then
        exit # not battery
      fi
      exec </dev/null
      exec ${pkgs.gawk}/bin/awk '
        function print_hm(h, m) {
          m = (h - int(h)) * 60
          return sprintf("%dh%dm", h, m)
        }

        function print_bar(n, r, t1, t2, t_col) {
          t1 = int(r * n)
          t2 = n - t1
          if (r >= .42)     t_col = "1;32"
          else if (r >= 23) t_col = "1;33"
          else if (r >= 11) t_col = "1;31"
          else              t_col = "5;1;31"
          return sgr(t_col) strdup("■", t1) sgr(";30") strdup("■", t2) sgr()
        }

        function sgr(p) {
          return "\x1b[" p "m"
        }

        function strdup(s,n,t) {
          t = sprintf("%"n"s","")
          gsub(/ /,s,t)
          return t
        }

        END {
          voltage_unit = "V"
          voltage_now = ENVIRON["POWER_SUPPLY_VOLTAGE_NOW"] / 10^6
          voltage_min_design = ENVIRON["POWER_SUPPLY_VOLTAGE_MIN_DESIGN"] / 10^6

          current_unit = "A"
          current_now = ENVIRON["POWER_SUPPLY_CURRENT_NOW"] / 10^6

          power_unit = "W"
          power_now = current_now * voltage_now

          charge_unit = "Ah"
          charge_now = ENVIRON["POWER_SUPPLY_CHARGE_NOW"] / 10^6
          charge_full = ENVIRON["POWER_SUPPLY_CHARGE_FULL"] / 10^6
          charge_ratio = charge_now / charge_full

          energy_unit = "Wh"
          energy_full = charge_full * voltage_min_design

          printf "%s %s %d%% %.2f%s/%.2f%s %d%s/%.1f%s %s\n" \
            , ENVIRON["POWER_SUPPLY_NAME"] \
            , print_bar(10, charge_ratio) \
            , charge_ratio * 100 \
            , charge_now, charge_unit \
            , current_now, current_unit \
            , energy_full, energy_unit \
            , power_now, power_unit \
            , print_hm(charge_now / current_now)
        }
      '
    '';
  in ''
    for uevent in /sys/class/power_supply/*/uevent; do
      ${power_supply} "$uevent"
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
  (${q-todo}) || :
  if [ "$PWD" != "$HOME" ]; then
    (HOME=$PWD; ${q-todo}) || :
  fi
  echo
  ${q-cal}
  echo
  ${q-isodate}
  ${q-sgtdate}
  (${q-gitdir}) &
  (${q-intel_backlight}) &
  (${q-power_supply}) &
  (${q-virtualization}) &
  (${q-wireless}) &
  (${q-online}) &
  (${q-thermal_zone}) &
  wait
''

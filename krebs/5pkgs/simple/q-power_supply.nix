{ gawk, gnused, writeDashBin }:

writeDashBin "q-power_supply" ''
  power_supply() {(
    set -efu
    uevent=$1
    eval "$(${gnused}/bin/sed -n '
      s/^\([A-Z_]\+=[0-9A-Za-z_-]*\)$/export \1/p
    ' $uevent)"
    case $POWER_SUPPLY_NAME in
      AC|Mains)
        exit # not battery
        ;;
    esac
    exec </dev/null
    exec ${gawk}/bin/awk '
      function die(s) {
        printf "%s: %s\n", name, s
        exit 1
      }

      function print_hm(h, m) {
        m = (h - int(h)) * 60
        return sprintf("%dh%dm", h, m)
      }

      function print_bar(r) {
        return \
          (r >= .1 ? bar_gradient[0] : bar_background) "■" \
          (r >= .2 ? bar_gradient[1] : bar_background) "■" \
          (r >= .3 ? bar_gradient[2] : bar_background) "■" \
          (r >= .4 ? bar_gradient[3] : bar_background) "■" \
          (r >= .5 ? bar_gradient[4] : bar_background) "■" \
          (r >= .6 ? bar_gradient[5] : bar_background) "■" \
          (r >= .7 ? bar_gradient[6] : bar_background) "■" \
          (r >= .8 ? bar_gradient[7] : bar_background) "■" \
          (r >= .9 ? bar_gradient[8] : bar_background) "■" \
          (r >=  1 ? bar_gradient[9] : bar_background) "■" \
          sgr()
      }

      function rgb(r, g, b) {
        return sgr("38;2;" r ";" g ";" b)
      }

      function sgr(p) {
        return "\x1b[" p "m"
      }

      BEGIN {
        bar_gradient[0] = rgb(216, 100,  83)
        bar_gradient[1] = rgb(210, 113,  72)
        bar_gradient[2] = rgb(201, 125,  65)
        bar_gradient[3] = rgb(190, 137,  63)
        bar_gradient[4] = rgb(178, 148,  67)
        bar_gradient[5] = rgb(166, 158,  75)
        bar_gradient[6] = rgb(153, 167,  88)
        bar_gradient[7] = rgb(140, 174, 104)
        bar_gradient[8] = rgb(127, 181, 122)
        bar_gradient[9] = rgb(116, 187, 141)
        bar_background  = rgb( 64,  64,  64)
      }

      END {
        name = ENVIRON["POWER_SUPPLY_NAME"]

        charge_unit = "Ah"
        charge_now = ENVIRON["POWER_SUPPLY_CHARGE_NOW"] / 10^6
        charge_full = ENVIRON["POWER_SUPPLY_CHARGE_FULL"] / 10^6

        current_unit = "A"
        current_now = ENVIRON["POWER_SUPPLY_CURRENT_NOW"] / 10^6

        energy_unit = "Wh"
        energy_now = ENVIRON["POWER_SUPPLY_ENERGY_NOW"] / 10^6
        energy_full = ENVIRON["POWER_SUPPLY_ENERGY_FULL"] / 10^6

        power_unit = "W"
        power_now = ENVIRON["POWER_SUPPLY_POWER_NOW"] / 10^6

        voltage_unit = "V"
        voltage_now = ENVIRON["POWER_SUPPLY_VOLTAGE_NOW"] / 10^6
        voltage_min_design = ENVIRON["POWER_SUPPLY_VOLTAGE_MIN_DESIGN"] / 10^6

        #printf "charge_now: %s\n", charge_now
        #printf "charge_full: %s\n", charge_full
        #printf "current_now: %s\n", current_now
        #printf "energy_now: %s\n", energy_now
        #printf "energy_full: %s\n", energy_full
        #printf "energy_full: %s\n", ENVIRON["POWER_SUPPLY_ENERGY_FULL"]
        #printf "energy_full: %s\n", ENVIRON["POWER_SUPPLY_ENERGY_FULL"] / 10^6
        #printf "power_now: %s\n", power_now
        #printf "voltage_now: %s\n", voltage_now

        if (current_now == 0 && voltage_now != 0) {
          current_now = power_now / voltage_now
        }
        if (power_now == 0) {
          power_now = current_now * voltage_now
        }
        if (charge_now == 0 && voltage_min_design != 0) {
          charge_now = energy_now / voltage_min_design
        }
        if (energy_now == 0) {
          energy_now = charge_now * voltage_min_design
        }
        if (charge_full == 0 && voltage_min_design != 0) {
          charge_full = energy_full / voltage_min_design
        }
        if (energy_full == 0) {
          energy_full = charge_full * voltage_min_design
        }

        if (charge_now == 0 || charge_full == 0) {
          die("unknown charge")
        }

        charge_ratio = charge_now / charge_full

        out = out name
        out = out sprintf(" %s", print_bar(charge_ratio))
        out = out sprintf(" %d%", charge_ratio * 100)
        out = out sprintf(" %.2f%s", charge_now, charge_unit)
        if (current_now != 0) {
          out = out sprintf("/%.1f%s", current_now, current_unit)
        }
        out = out sprintf(" %d%s", energy_full, energy_unit)
        if (power_now != 0) {
          out = out sprintf("/%.1f%s", power_now, power_unit)
        }
        if (current_now != 0) {
          out = out sprintf(" %s", print_hm(charge_now / current_now))
        }

        print out
      }
    '
  )}

  for uevent in /sys/class/power_supply/*/uevent; do
    power_supply "$uevent" || :
  done
''

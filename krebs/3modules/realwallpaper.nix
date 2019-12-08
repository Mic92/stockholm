{ config, lib, pkgs, ... }:

with import <stockholm/lib>;
let
  cfg = config.krebs.realwallpaper;

  out = {
    options.krebs.realwallpaper = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "realwallpaper";

    workingDir = mkOption {
      type = types.str;
      default = "/var/realwallpaper/";
    };

    nightmap = mkOption {
      type = types.str;
      default = "http://eoimages.gsfc.nasa.gov/images/imagerecords/55000/55167/earth_lights_lrg.jpg";
    };

    daymap = mkOption {
      type = types.str;
      default = "https://www.nnvl.noaa.gov/images/globaldata/SnowIceCover_Daily.png";
    };

    cloudmap = mkOption {
      type = types.str;
      default = "http://home.megapass.co.kr/~holywatr/cloud_data/clouds_2048.jpg";
    };

    marker = mkOption {
      type = types.str;
      default = "http://graph.r/marker.json";
    };

    timerConfig = mkOption {
      type = types.unspecified;
      default = {
        OnCalendar = "*:0/15";
      };
    };
  };

  imp = {
    systemd.timers.realwallpaper = {
      description = "real wallpaper generator timer";
      wantedBy = [ "timers.target" ];

      timerConfig = cfg.timerConfig;
    };

    systemd.services.realwallpaper = {
      description = "real wallpaper generator";
      after = [ "network.target" ];

      path = with pkgs; [
        xplanet
        imagemagick
        curl
        file
        jq
      ];

      environment = {
        working_dir = cfg.workingDir;
        nightmap_url = cfg.nightmap;
        daymap_url = cfg.daymap;
        cloudmap_url = cfg.cloudmap;
        marker_url = cfg.marker;
      };

      restartIfChanged = true;

      serviceConfig = {
        Type = "simple";
        ExecStart = pkgs.writeDash "generate-wallpaper" ''
          set -euf

          # usage: getimg FILENAME URL
          fetch() {
            echo "fetch $1"
            curl -LsS -z "$1" -o "$1" "$2"
          }

          # usage: check_type FILENAME TYPE
          check_type() {
            if ! file -ib "$1" | grep -q "^$2/"; then
              echo "$1 is not of type $2" >&2
              rm "$1"
              return 1
            fi
          }

          # usage: image_size FILENAME
          image_size() {
            identify "$1" | awk '{print$3}'
          }

          # usage: make_mask DST SRC MASK
          make_layer() {
            if needs_rebuild "$@"; then
              echo "make $1 (apply mask)" >&2
              convert "$2" "$3" -alpha off -compose copy_opacity -composite "$1"
            fi
          }

          # usage: flatten DST HILAYER LOLAYER
          flatten() {
            if needs_rebuild "$@"; then
              echo "make $1 (flatten)" >&2
              composite "$2" "$3" "$1"
            fi
          }

          # usage: needs_rebuild DST SRC...
          needs_rebuild() {
            a="$1"
            shift
            if ! test -e "$a"; then
              #echo "  $a does not exist" >&2
              result=0
            else
              result=1
              for b; do
                if test "$b" -nt "$a"; then
                  #echo "  $b is newer than $a" >&2
                  result=0
                fi
              done
            fi
            #case $result in
            #  0) echo "$a needs rebuild" >&2;;
            #esac
            return $result
          }

          main() {
            cd ${cfg.workingDir}

            # fetch source images in parallel
            fetch nightmap-raw.jpg \
              ${cfg.nightmap} &
            fetch daymap-raw.png \
              ${cfg.daymap} &
            fetch clouds-raw.jpg \
              ${cfg.cloudmap} &
            fetch marker.json \
              ${cfg.marker} &
            wait

            check_type nightmap-raw.jpg image
            check_type daymap-raw.png image
            check_type clouds-raw.jpg image

            in_size=2048x1024
            xplanet_out_size=1466x1200
            out_geometry=1366x768+100+160

            nightsnow_color='#0c1a49'  # nightmap

            for raw in \
                nightmap-raw.jpg \
                daymap-raw.png \
                clouds-raw.jpg \
                ;
            do
              normal=''${raw%-raw.*}.png
              if needs_rebuild $normal $raw; then
                echo "make $normal; normalize $raw" >&2
                convert $raw -scale $in_size $normal
              fi
            done

            # create nightmap-fullsnow
            if needs_rebuild nightmap-fullsnow.png; then
              convert -size $in_size xc:$nightsnow_color nightmap-fullsnow.png
            fi

            # extract daymap-snowmask from daymap-final
            if needs_rebuild daymap-snowmask.png daymap.png; then
              convert daymap.png -threshold 95% daymap-snowmask.png
            fi

            # extract nightmap-lightmask from nightmap
            if needs_rebuild nightmap-lightmask.png nightmap.png; then
              convert nightmap.png -threshold 25% nightmap-lightmask.png
            fi

            # create layers
            make_layer nightmap-snowlayer.png nightmap-fullsnow.png daymap-snowmask.png
            make_layer nightmap-lightlayer.png nightmap.png nightmap-lightmask.png

            # apply layers
            flatten nightmap-lightsnowlayer.png \
              nightmap-lightlayer.png \
              nightmap-snowlayer.png

            flatten nightmap-final.png \
              nightmap-lightsnowlayer.png \
              nightmap.png

            # create marker file from json
            if [ -s marker.json ]; then
              jq -r 'to_entries[] | @json "\(.value.latitude) \(.value.longitude)"' marker.json > marker_file
            fi

            # make all unmodified files as final
            for normal in \
                daymap.png \
                clouds.png \
                ;
            do
              final=''${normal%.png}-final.png
              needs_rebuild $final &&
                ln $normal $final
            done

            # rebuild every time to update shadow
            xplanet --num_times 1 --geometry $xplanet_out_size \
              --output xplanet-output.png --projection merc \
              -config ${pkgs.writeText "xplanet.config" ''
                [earth]
                "Earth"
                map=daymap-final.png
                night_map=nightmap-final.png
                cloud_map=clouds-final.png
                cloud_threshold=10
                shade=15
              ''}

            xplanet --num_times 1 --geometry $xplanet_out_size \
              --output xplanet-krebs-output.png --projection merc \
              -config ${pkgs.writeText "xplanet-krebs.config" ''
                [earth]
                "Earth"
                map=daymap-final.png
                night_map=nightmap-final.png
                cloud_map=clouds-final.png
                cloud_threshold=10
                marker_file=marker_file
                shade=15
              ''}

            # trim xplanet output
            if needs_rebuild realwallpaper.png xplanet-output.png; then
              convert xplanet-output.png -crop $out_geometry \
                realwallpaper-tmp.png
                mv realwallpaper-tmp.png realwallpaper.png
            fi

            if needs_rebuild realwallpaper-krebs.png xplanet-krebs-output.png; then
              convert xplanet-krebs-output.png -crop $out_geometry \
                realwallpaper-krebs-tmp.png
                mv realwallpaper-krebs-tmp.png realwallpaper-krebs.png
            fi
          }

          main "$@"
        '';
        User = "realwallpaper";
      };
    };

    users.extraUsers.realwallpaper = {
      uid = genid "realwallpaper";
      home = cfg.workingDir;
      createHome = true;
    };
  };

in
out


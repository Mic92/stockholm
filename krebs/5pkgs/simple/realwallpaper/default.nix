{ pkgs, ... }:
pkgs.writers.writeDashBin "generate-wallpaper" ''
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

  # check if file exists and fetch only if missing
  fetch_once() {
    name=$1
    url=$2
    test -e "$name" || fetch "$name" "$url"
  }

  fetch_older_min() {
    min=$1
    name=$2
    url=$3
    if ! test "$(find $name -mmin -$min)"; then
      fetch "$name" "$url"
    fi
  }

  fetch_older_days() {
    days=$1
    name=$2
    url=$3
    if ! test "$(find $name -mmin -$days)"; then
      fetch "$name" "$url"
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

  get_neo_url() {
    url=$1
    curl -Ss "$url" | grep '3600 x 1800' | sed 's/.*href="\([^"]*\)".*/\1/'
  }

  main() {
    cd "$working_dir"

    # fetch source images in parallel
    fetch_once sun-raw.png \
      'http://simpleicon.com/wp-content/uploads/sun-64x64.png' &
    fetch_once moon-raw.png \
      'http://simpleicon.com/wp-content/uploads/moon__star-64x64.png' &
    fetch_once nightmap-raw.jpg \
      'https://eoimages.gsfc.nasa.gov/images/imagerecords/144000/144898/BlackMarble_2016_3km.jpg' &
    fetch_once daymap-raw.tif \
      'https://eoimages.gsfc.nasa.gov/images/imagerecords/57000/57752/land_shallow_topo_8192.tif' &

    fetch_once mercury-raw.svg \
      'https://upload.wikimedia.org/wikipedia/commons/2/2e/Mercury_symbol.svg' &
    fetch_once venus-raw.svg \
      'https://upload.wikimedia.org/wikipedia/commons/6/66/Venus_symbol.svg' &
    fetch_once mars-raw.svg \
      'https://upload.wikimedia.org/wikipedia/commons/b/b7/Mars_symbol.svg' &
    fetch_once jupiter-raw.svg \
      'https://upload.wikimedia.org/wikipedia/commons/2/26/Jupiter_symbol.svg' &
    fetch_once saturn-raw.svg \
      'https://upload.wikimedia.org/wikipedia/commons/7/74/Saturn_symbol.svg' &
    fetch_once uranus-raw.svg \
      'https://upload.wikimedia.org/wikipedia/commons/f/f1/Uranus_symbol.svg' &
    fetch_once neptune-raw.svg \
      'https://upload.wikimedia.org/wikipedia/commons/4/47/Neptune_symbol.svg' &

    fetch_once krebs-raw.svg \
      'https://raw.githubusercontent.com/krebs/painload/master/cholerab/bling/krebs_aquarium.svg' &

    fetch_older_min 720 ice-raw.jpg $(get_neo_url \
      'https://neo.sci.gsfc.nasa.gov/view.php?datasetId=NISE_D') &
    fetch_older_days 3 snow-raw.jpg $(get_neo_url \
      'https://neo.sci.gsfc.nasa.gov/view.php?datasetId=MOD10C1_E_SNOW') &
    fetch_older_days 7 chlora-raw.jpg $(get_neo_url \
      'https://neo.sci.gsfc.nasa.gov/view.php?datasetId=MY1DMM_CHLORA') &
    fetch_older_days 3 fire-raw.jpg $(get_neo_url \
      'https://neo.sci.gsfc.nasa.gov/view.php?datasetId=MOD14A1_E_FIRE') &

    # regular fetches
    fetch marker.json "$marker_url" &

    wait

    # fetch clouds if they are older than 3h
    if ! test "$(find clouds-raw.png -mmin -180)"; then
      ${pkgs.nomads-cloud}/bin/nomads-cloud clouds-raw.png
    fi

    check_type sun-raw.png image
    check_type moon-raw.png image
    check_type nightmap-raw.jpg image
    check_type daymap-raw.tif image
    check_type ice-raw.jpg image
    check_type snow-raw.jpg image
    check_type chlora-raw.jpg image
    check_type fire-raw.jpg image
    check_type clouds-raw.png image

    check_type mercury-raw.svg image
    check_type venus-raw.svg image
    check_type mars-raw.svg image
    check_type jupiter-raw.svg image
    check_type saturn-raw.svg image
    check_type uranus-raw.svg image
    check_type neptune-raw.svg image

    check_type krebs-raw.svg image

    in_size=3600x1800
    xplanet_out_size=3600x2950
    out_geometry=3200x1800+300+400

    for raw in \
        nightmap-raw.jpg \
        daymap-raw.tif \
        snow-raw.jpg \
        chlora-raw.jpg \
        clouds-raw.png \
        ;
    do
      normal=''${raw%-raw.*}.png
      if needs_rebuild $normal $raw; then
        echo "make $normal; normalize $raw" >&2
        convert $raw -scale $in_size $normal
      fi
    done

    # remove snow from ice map
    if needs_rebuild ice.png ice-raw.jpg; then
      convert ice-raw.jpg -fuzz 20% -fill black -opaque white -scale "$in_size" ice.png
    fi

    # make fire more red
    if needs_rebuild fire.png fire-raw.jpg; then
      convert fire-raw.jpg -fuzz 20% -fill '#ef840c' -opaque white -scale "$in_size" fire.png
    fi

    if needs_rebuild sun.png sun-raw.png; then
      convert sun-raw.png -fill gold -opaque black PNG64:sun.png
    fi

    if needs_rebuild moon.png moon-raw.png; then
      convert moon-raw.png -fill royalblue -opaque black PNG64:moon.png
    fi

    # -- Planets --

    if needs_rebuild mercury.png mercury-raw.svg; then
      sed -i 's/#000/#A6A6A6/g' mercury-raw.svg
      inkscape -z -e mercury.png -w 40 -h 40 mercury-raw.svg
    fi

    if needs_rebuild venus.png venus-raw.svg; then
      sed -i 's/#000/#C40B98/g' venus-raw.svg
      inkscape -z -e venus.png -w 40 -h 40 venus-raw.svg
    fi

    if needs_rebuild mars.png mars-raw.svg; then
      sed -i 's/#000/#E35A5C/g' mars-raw.svg
      inkscape -z -e mars.png -w 40 -h 40 mars-raw.svg
    fi

    if needs_rebuild jupiter.png jupiter-raw.svg; then
      sed -i 's/#000/#DEA182/g' jupiter-raw.svg
      inkscape -z -e jupiter.png -w 40 -h 40 jupiter-raw.svg
    fi

    if needs_rebuild saturn.png saturn-raw.svg; then
      sed -i 's/#000/#E4C282/g' saturn-raw.svg
      inkscape -z -e saturn.png -w 40 -h 40 saturn-raw.svg
    fi

    if needs_rebuild uranus.png uranus-raw.svg; then
      sed -i 's/#000/#97BCC2/g' uranus-raw.svg
      inkscape -z -e uranus.png -w 40 -h 40 uranus-raw.svg
    fi

    if needs_rebuild neptune.png neptune-raw.svg; then
      sed -i 's/#000/#274687/g' neptune-raw.svg
      inkscape -z -e neptune.png -w 40 -h 40 neptune-raw.svg
    fi

    if needs_rebuild krebs.png krebs-raw.svg; then
      inkscape -z -e krebs.png -w 16 -h 16 krebs-raw.svg
    fi

    # -- Daymap --

    # merge with water chlora layer
    if needs_rebuild daymap-final.png daymap.png fire.png snow.png ice.png chlora.png; then
      convert daymap.png fire.png -compose lighten -composite daymap-1.png
      convert daymap-1.png ice.png -compose lighten -composite daymap-2.png
      convert daymap-2.png snow.png -compose lighten -composite daymap-3.png
      convert daymap-3.png chlora.png -compose lighten -composite daymap-final.png
    fi

    # -- Nightmap --

    if needs_rebuild nightmap-final.png nightmap.png fire.png snow.png ice.png chlora.png; then
      convert nightmap.png fire.png -compose lighten -composite nightmap-1.png
      convert nightmap-1.png \( -fill black -colorize 70% ice.png \) -compose lighten -composite nightmap-2.png
      convert nightmap-2.png \( -fill black -colorize 70% snow.png \) -compose lighten -composite nightmap-3.png
      convert nightmap-3.png \( -fill black -colorize 70% chlora.png \) -compose lighten -composite nightmap-final.png
    fi

    # create marker file from json
    if [ -s marker.json ]; then
      jq -r 'to_entries[] | @json "\(.value.latitude) \(.value.longitude) image=krebs.png"' marker.json > marker_file
      echo 'position=sun image=sun.png' >> marker_file
      echo 'position=moon image=moon.png' >> marker_file
      echo 'position=mercury image=mercury.png' >> marker_file
      echo 'position=venus image=venus.png' >> marker_file
      echo 'position=mars image=mars.png' >> marker_file
      echo 'position=jupiter image=jupiter.png' >> marker_file
      echo 'position=uranus image=uranus.png' >> marker_file
      echo 'position=neptune image=neptune.png' >> marker_file
    fi

    # rebuild every time to update shadow
    xplanet --num_times 1 --geometry $xplanet_out_size \
      --output xplanet-output.png --projection merc \
      -config ${pkgs.writeText "xplanet.config" ''
        [earth]
        "Earth"
        map=daymap-final.png
        night_map=nightmap-final.png
        cloud_map=clouds.png
        cloud_threshold=1
        cloud_gamma=2.5
        shade=15
      ''}

    xplanet --num_times 1 --geometry $xplanet_out_size \
      --output xplanet-krebs-output.png --projection merc \
      -config ${pkgs.writeText "xplanet-krebs.config" ''
        [earth]
        "Earth"
        map=daymap-final.png
        night_map=nightmap-final.png
        cloud_map=clouds.png
        cloud_threshold=1
        cloud_gamma=2.5
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
''

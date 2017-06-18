{ jq, gnused, writeBashBin, ... }:

writeBashBin "urlencode" ''
  set -efu

  decode() {
    printf %b "$(${gnused}/bin/sed 's/ /+/g; s/%/\\x/g')"
  }

  encode() {
    ${jq}/bin/jq -Rr '@uri "\(.)"'
  }

  # shellcheck disable=SC2048
  case $* in
    -d) decode;;
    "") encode;;
    *)
      echo "$0: error: your argument is invalid" >&2
      exit 1
  esac
''

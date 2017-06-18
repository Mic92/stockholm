{ pkgs, ... }:

pkgs.writeDashBin "krebszones" ''
  set -efu
  export OVH_ZONE_CONFIG=$HOME/.secrets/krebs/ovh-zone.conf
  case $* in
    import)
      set -- import /etc/zones/krebsco.de krebsco.de
      echo "+ krebszones $*" >&2
      ;;
  esac
  exec ${pkgs.ovh-zone}/bin/ovh-zone "$@"
''

. ./lib/url.sh

_cac_exec() {
  if test -z "${cac_via-}"; then
    (exec "$@")
  else
    ssh -q "$cac_via" -t "$@"
  fi
}

_cac_get_api_v1() {
  _cac_exec curl -fsS \
    $(shift
      set -- "$@" login="$cac_login" key="$cac_key"
      for arg; do
        echo -d $(printf '%s' "$arg" | url_encode)
      done
    ) \
    -G "https://panel.cloudatcost.com/api/v1/$1.php"
}

cac_listservers() {
  _cac_get_api_v1 listservers
}

cac_listtasks() {
  _cac_get_api_v1 listtasks
}

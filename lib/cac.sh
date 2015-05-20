. ./lib/url.sh

cac_listservers() {
  _cac_get_api_v1 listservers
}

cac_listtasks() {
  _cac_get_api_v1 listtasks
}

cac_listtemplates() {
  _cac_get_api_v1 listtemplates
}

cac_console() {
  _cac_post_api_v1 console sid="$1"
}

cac_powerop() {
  _cac_post_api_v1 powerop sid="$1" action="$2"
}

cac_renameserver() {
  _cac_post_api_v1 renameserver sid="$1" name="$2"
}

cac_rnds() {
  _cac_post_api_v1 rdns sid="$1" hostname="$2"
}

cac_runmode() {
  _cac_post_api_v1 rdns sid="$1" mode="$2"
}

# default os=26 is CentOS-7-64bit
cac_cloudpro_build() {
  _cac_post_api_v1 cloudpro/build cpu="$1" ram="$2" storage="$3" os="${4-26}"
}

cac_cloudpro_delete() {
  _cac_post_api_v1 cloudpro/delete sid="$1"
}

cac_cloudpro_resources() {
  _cac_get_api_v1 cloudpro/resources
}

_cac_get_api_v1() {
  _cac_curl_api_v1 -G "$@"
}

_cac_post_api_v1() {
  _cac_curl_api_v1 -XPOST "$@"
}

_cac_curl_api_v1() {
  _cac_exec curl -fsS "$1" "https://panel.cloudatcost.com/api/v1/$2.php" $(
    shift 2
    set -- "$@" login="$cac_login" key="$cac_key"
    for arg; do
      echo -d $(printf '%s' "$arg" | url_encode)
    done
  )
}

_cac_exec() {
  if test -z "${cac_via-}"; then
    (exec "$@")
  else
    ssh -q "$cac_via" -t "$@"
  fi
}

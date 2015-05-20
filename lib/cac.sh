_cac_exec() {
  if test -z "${cac_via-}"; then
    (exec "$@")
  else
    ssh -q "$cac_via" -t "$@"
  fi
}

cac_listservers() {
  _cac_exec \
    curl -fsS \
      -G \
      --data-urlencode key="$cac_key" \
      --data-urlencode login="$cac_login" \
      'https://panel.cloudatcost.com/api/v1/listservers.php'
}

cac_listtasks() {
  _cac_exec \
    curl -fsS \
      -G \
      --data-urlencode key="$cac_key" \
      --data-urlencode login="$cac_login" \
      'https://panel.cloudatcost.com/api/v1/listtasks.php'
}

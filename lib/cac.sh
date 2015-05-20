cac_listservers() {
  if test -z "${cac_via-}"; then
    curl -fsS \
      -G \
      --data-urlencode key="$cac_key" \
      --data-urlencode login="$cac_login" \
      'https://panel.cloudatcost.com/api/v1/listservers.php'
  else
    ssh -q $cac_via -t curl -fsS \
      -G \
      --data-urlencode key="$cac_key" \
      --data-urlencode login="$cac_login" \
      'https://panel.cloudatcost.com/api/v1/listservers.php'
  fi
}

cac_listtasks() {
  if test -z "${cac_via-}"; then
    curl -fsS \
      -G \
      --data-urlencode key="$cac_key" \
      --data-urlencode login="$cac_login" \
      'https://panel.cloudatcost.com/api/v1/listtasks.php'
  else
    ssh -q $cac_via -t curl -fsS \
      -G \
      --data-urlencode key="$cac_key" \
      --data-urlencode login="$cac_login" \
      'https://panel.cloudatcost.com/api/v1/listtasks.php'
  fi
}

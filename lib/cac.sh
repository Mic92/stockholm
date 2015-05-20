cac_listservers() {
  if test -z "${cac_via-}"; then
    curl -fsS \
      "https://panel.cloudatcost.com/api/v1/listservers.php?key=$cac_key&login=$cac_login"
  else
    ssh -q $cac_via -t curl -fsS \
      "https://panel.cloudatcost.com/api/v1/listservers.php?key=$cac_key\\&login=$cac_login"
  fi
}

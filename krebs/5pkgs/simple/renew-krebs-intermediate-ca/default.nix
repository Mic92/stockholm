{ pkgs }:
pkgs.writers.writeDashBin "renew-intermediate-ca" ''
  TMPDIR=$(mktemp -d)
  trap "rm -rf $TMPDIR;" INT TERM EXIT
  mkdir -p "$TMPDIR/krebs"
  brain show ca/ca.key > "$TMPDIR/krebs/ca.key"
  brain show ca/ca.crt > "$TMPDIR/krebs/ca.crt"
  brain show krebs-secrets/hotdog/acme_ca.key > "$TMPDIR/acme.key"
  cp ${toString ../../../6assets/krebsAcmeCA.crt} "$TMPDIR/acme.crt"
  export STEPPATH="$TMPDIR/step"
  cat << EOF > "$TMPDIR/intermediate.tpl"
  {
      "subject": {{ toJson .Subject }},
      "keyUsage": ["certSign", "crlSign"],
      "basicConstraints": {
          "isCA": true,
          "maxPathLen": 0
      },
      "nameConstraints": {
          "critical": true,
          "permittedDNSDomains": ["r" ,"w"]
      }
  }
  EOF

  ${pkgs.step-cli}/bin/step ca renew "$TMPDIR/ca.crt" "$TMPDIR/ca.key" \
    --offline \
    --root "$TMPDIR/krebs/ca.crt" \
    --ca-config "$TMPDIR/intermediate.tpl"
''

{ pkgs }:
pkgs.writeDashBin "l-gen-secrets" ''
  HOSTNAME="$1"
  TMPDIR=$(${pkgs.coreutils}/bin/mktemp -d)
  PASSWORD=$(${pkgs.pwgen}/bin/pwgen 25 1)
  HASHED_PASSWORD=$(echo $PASSWORD | ${pkgs.hashPassword}/bin/hashPassword -s) > /dev/null

  ${pkgs.openssh}/bin/ssh-keygen -t ed25519 -f $TMPDIR/ssh.id_ed25519 -P "" -C "" >/dev/null
  ${pkgs.openssl}/bin/openssl genrsa -out $TMPDIR/retiolum.rsa_key.priv 4096 2>/dev/null > /dev/null
  ${pkgs.openssl}/bin/openssl rsa -in $TMPDIR/retiolum.rsa_key.priv -pubout -out $TMPDIR/retiolum.rsa_key.pub 2>/dev/null > /dev/null
  ${pkgs.wireguard-tools}/bin/wg genkey > $TMPDIR/wiregrill.key
  ${pkgs.coreutils}/bin/cat $TMPDIR/wiregrill.key | ${pkgs.wireguard-tools}/bin/wg pubkey > $TMPDIR/wiregrill.pub
  cat <<EOF > $TMPDIR/hashedPasswords.nix
  {
    root = "$HASHED_PASSWORD";
    mainUser = "$HASHED_PASSWORD";
  }
  EOF

  cd $TMPDIR
  for x in *; do
    ${pkgs.coreutils}/bin/cat $x | ${pkgs.pass}/bin/pass insert -m hosts/$HOSTNAME/$x > /dev/null
  done
  echo $PASSWORD | ${pkgs.pass}/bin/pass insert -m admin/$HOSTNAME/pass > /dev/null

  cat <<EOF
    $HOSTNAME = {
      nets = {
        retiolum = {
          ip4.addr = "10.243.0.changeme";
          ip6.addr = r6 "changeme";
          aliases = [
            "$HOSTNAME.r"
          ];
          tinc.pubkey = ${"''"}
  $(cat $TMPDIR/retiolum.rsa_key.pub)
          ${"''"};
        };
        wiregrill = {
          ip6.addr = w6 "changeme";
          aliases = [
            "$HOSTNAME.w"
          ];
          wireguard.pubkey = ${"''"}
  $(cat $TMPDIR/wiregrill.pub)
          ${"''"};
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "$(cat $TMPDIR/ssh.id_ed25519.pub)";
    };
  EOF

  rm -rf $TMPDIR
''


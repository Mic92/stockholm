{ pkgs }:
pkgs.writers.writeDashBin "generate-secrets" ''
  set -eu
  HOSTNAME="$1"
  TMPDIR=$(${pkgs.coreutils}/bin/mktemp -d)
  cd $TMPDIR

  PASSWORD=$(${pkgs.pwgen}/bin/pwgen 25 1)
  HASHED_PASSWORD=$(echo $PASSWORD | ${pkgs.hashPassword}/bin/hashPassword -s) > /dev/null

  ${pkgs.openssh}/bin/ssh-keygen -t ed25519 -f $TMPDIR/ssh.id_ed25519 -P "" -C "" >/dev/null
  ${pkgs.tinc_pre}/bin/tinc --config "$TMPDIR" generate-keys 4096 >/dev/null
  cat <<EOF > $TMPDIR/hashedPasswords.nix
  {
    root = "$HASHED_PASSWORD";
  }
  EOF

  for x in *; do
    ${pkgs.coreutils}/bin/cat $x | ${pkgs.brain}/bin/brain insert -m krebs-secrets/$HOSTNAME/$x > /dev/null
  done
  echo $PASSWORD | ${pkgs.brain}/bin/brain insert -m hosts/$HOSTNAME/root > /dev/null

  cat <<EOF
    $HOSTNAME = {
      cores = 1;
      owner = config.krebs.users.krebs;
      nets = {
        retiolum = {
          ip4.addr = "10.243.0.changeme";
          ip6.addr = "42:0:0:0:0:0:0:changeme";
          aliases = [
            "$HOSTNAME.r"
          ];
          tinc = {
            pubkey = ${"''"}
  $(cat $TMPDIR/rsa_key.pub)
            ${"''"};
            pubkey_ed25519 = "$(cut -d ' ' -f 3 $TMPDIR/ed25519_key.pub)";
          };
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "$(cat $TMPDIR/ssh.id_ed25519.pub)";
    };
  EOF

  rm -rf $TMPDIR
''


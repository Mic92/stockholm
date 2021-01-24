{ pkgs, lib }:

#usage: ecrypt mount /var/crypted /var/unencrypted
pkgs.writers.writeDashBin "ecrypt" ''
  set -euf

  PATH=${lib.makeBinPath (with pkgs; [
    coreutils
    ecryptfs
    gnused
    gnugrep
    jq
    mount
    keyutils
    umount
  ])}

  # turn echo back on if killed
  trap 'stty echo' INT

  case "$1" in
    init)
      shift
      mkdir -p "$1" "$2"

      # abort if src or dest are not empty
      if [ -e "$1"/.cfg.json ]; then
        echo 'source dir is already configured, aborting'
        exit 1
      elif ls -1qA "$2" | grep -q .; then
        echo 'destination dir is not empty, aborting'
        exit 1
      else
        # we start and exit ecryptfs-manager again to circumvent a bug where mounting the ecryptfs fails
        echo 4 | ecryptfs-manager
        stty -echo
        printf "passphrase: "
        read  passphrase
        stty echo
        sig=$(echo "$passphrase" | ecryptfs-add-passphrase | grep 'Inserted auth tok' | sed 's/.*\[\(.*\)\].*/\1/')
        mount -t ecryptfs \
          -o ecryptfs_unlink_sigs,ecryptfs_fnek_sig="$sig",ecryptfs_key_bytes=16,ecryptfs_cipher=aes,ecryptfs_sig="$sig" \
          "$1" "$2"

        # add sig to json state file
        jq -n --arg sig "$sig" '{ "sig": $sig }' > "$1"/.cfg.json
      fi
      ;;

    mount)
      shift
      if ! [ -e "$1"/.cfg.json ]; then
        echo '.cfg.json missing in src'
        exit 1
      fi
      old_sig=$(cat "$1"/.cfg.json | jq -r .sig)

      # check if key is already in keyring, otherwise add it
      
      if keyctl list @u | grep -q "$old_sig"; then
        echo 'pw already saved'
      else
        # we start and exit ecryptfs-manager again to circumvent a bug where mounting the ecryptfs fails
        echo 4 | ecryptfs-manager
        stty -echo
        printf "passphrase: "
        read  passphrase
        stty echo
        new_sig=$(echo "$passphrase" | ecryptfs-add-passphrase | grep 'Inserted auth tok' | sed 's/.*\[\(.*\)\].*/\1/')

        # check if passphrase matches sig
        if [ "$old_sig" != "$new_sig" ]; then
          echo 'passphrase does not match sig, bailing out'
          new_keyid=$(keyctl list @u | grep "$new_sig" | sed 's/\([0-9]*\).*/\1/')
          keyctl revoke "$new_keyid"
          keyctl unlink "$new_keyid"
          exit 1
        fi
      fi

      sig=$old_sig
      keyid=$(keyctl list @u | grep "$sig" | sed 's/\([0-9]*\).*/\1/')
      if (ls -1qA "$2" | grep -q .); then
        echo 'destination is not empty, bailing out'
        exit 1
      else
        mount -i -t ecryptfs \
          -o ecryptfs_passthrough=no,verbose=no,ecryptfs_unlink_sigs,ecryptfs_fnek_sig="$sig",ecryptfs_key_bytes=16,ecryptfs_cipher=aes,ecryptfs_sig="$sig" \
          "$1" "$2"
      fi
      ;;

    unmount)
      shift

      sig=$(cat "$1"/.cfg.json | jq -r .sig)
      keyid=$(keyctl list @u | grep "$sig" | sed 's/\s*\([0-9]*\).*/\1/')

      umount "$2" || :
      keyctl revoke "$keyid"
      keyctl unlink "$keyid"
      ;;

    *)
      echo 'usage:
        ecrypt init /tmp/src/ /tmp/dst/
        ecrypt mount /tmp/src/ /tmp/dst/
        ecrypt unmount /tmp/src/ /tmp/dst/
      '
  esac
''

{ pkgs, lib, pubkey ? "", disk ? "/dev/sda", vgname ? "pool", luksmap ? "luksmap", keyfile ? "/root/keyfile", ... }:

with lib;

pkgs.writeText "init" ''
  #! /bin/sh
  # usage: curl xu/~tv/init | sh
  set -efu
  # TODO nix-env -f '<nixpkgs>' -iA jq # if not exists (also version)
  #       install at tmp location


  case $(cat /proc/cmdline) in
     *' root=LABEL=NIXOS_ISO '*) :;;
     *) echo Error: unknown operating system >&2; exit 1;;
  esac

  keyfile=${keyfile}

  disk=${disk}

  luksdev=${disk}3
  luksmap=/dev/mapper/${luksmap}

  vgname=${vgname}

  bootdev=/dev/sda2

  rootdev=/dev/mapper/${vgname}-root
  homedev=/dev/mapper/${vgname}-home

  #
  #generate keyfile
  #

  if ! test -e "$keyfile"; then
    dd if=/dev/urandom bs=512 count=2048 of=$keyfile
  fi

  #
  # partitioning
  #

  # http://en.wikipedia.org/wiki/GUID_Partition_Table
  # undo:
  #   dd if=/dev/zero bs=512 count=34 of=/dev/sda
  # TODO zero last 34 blocks (lsblk -bno SIZE /dev/sda)
  if ! test "$(blkid -o value -s PTTYPE "$disk")" = gpt; then
    parted -a optimal "$disk" \
        mklabel gpt \
        mkpart no-fs 0 1024KiB \
        set 1 bios_grub on \
        mkpart ESP fat32 1025KiB 1024MiB  set 2 boot on \
        mkpart primary 1025MiB 100%
  fi

  if ! test "$(blkid -o value -s PARTLABEL "$luksdev")" = primary; then
    echo zonk2
    exit 23
  fi

  if ! cryptsetup isLuks "$luksdev"; then
    # aes xts-plain64
    cryptsetup luksFormat "$luksdev" "$keyfile" \
        -h sha512 \
        --iter-time 5000
  fi

  if ! test -e "$luksmap"; then
    cryptsetup luksOpen "$luksdev" "$(basename "$luksmap")" \
        --key-file "$keyfile"
  fi
  # cryptsetup close

  if ! test "$(blkid -o value -s TYPE "$luksmap")" = LVM2_member; then
    pvcreate "$luksmap"
  fi

  if ! vgdisplay -s "$vgname"; then vgcreate "$vgname" "$luksmap"; fi

  lvchange -a y /dev/mapper/"$vgname"

  if ! test -e "$rootdev"; then lvcreate -L 7G -n root "$vgname"; fi
  if ! test -e "$homedev"; then lvcreate -L 100M -n home "$vgname"; fi

  # lvchange -a n "$vgname"


  #
  # formatting
  #

  if ! test "$(blkid -o value -s TYPE "$bootdev")" = vfat; then
    mkfs.vfat "$bootdev"
  fi

  if ! test "$(blkid -o value -s TYPE "$rootdev")" = btrfs; then
    mkfs.btrfs "$rootdev"
  fi

  if ! test "$(blkid -o value -s TYPE "$homedev")" = btrfs; then
    mkfs.btrfs "$homedev"
  fi


  if ! test "$(lsblk -n -o MOUNTPOINT "$rootdev")" = /mnt; then
    mount "$rootdev" /mnt
  fi
  if ! test "$(lsblk -n -o MOUNTPOINT "$bootdev")" = /mnt/boot; then
    mkdir -m 0000 -p /mnt/boot
    mount "$bootdev" /mnt/boot
  fi
  if ! test "$(lsblk -n -o MOUNTPOINT "$homedev")" = /mnt/home; then
    mkdir -m 0000 -p /mnt/home
    mount "$homedev" /mnt/home
  fi

  # umount -R /mnt

  #
  # dependencies for stockholm
  #

  nix-env -iA nixos.git

  # TODO: get sentinal file from target_path
  mkdir -p /mnt/var/src
  touch /mnt/var/src/.populate

  #
  # print all the infos
  #

  parted "$disk" print
  lsblk "$disk"

  key='${pubkey}'
  if [ "$(cat /root/.ssh/authorized_keys 2>/dev/null)" != "$key" ]; then
    mkdir -p /root/.ssh
    echo "$key" > /root/.ssh/authorized_keys
  fi
  systemctl start sshd
  ip route
  echo READY.
''

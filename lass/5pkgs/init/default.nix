{ pkgs, lib, vgname ? "vgname", luksmap ? "luksmap", ... }:

with lib;

pkgs.writeScript "init" ''
  #!/usr/bin/env nix-shell
  #! nix-shell -i bash -p jq parted libxfs
  set -efu

  disk=$1

  if mount | grep -q "$disk"; then
    echo "target device is already mounted, bailout"
    exit 2
  fi

  luksdev="$disk"3
  luksmap=/dev/mapper/${luksmap}

  vgname=${vgname}

  bootdev=/dev/sda2

  rootdev=/dev/mapper/${vgname}-root
  homedev=/dev/mapper/${vgname}-home

  read -p "LUKS Password: " lukspw

  #
  # partitioning
  #

  # http://en.wikipedia.org/wiki/GUID_Partition_Table
  # undo:
  #   dd if=/dev/zero bs=512 count=34 of=/dev/sda
  # TODO zero last 34 blocks (lsblk -bno SIZE /dev/sda)
  if ! test "$(blkid -o value -s PTTYPE "$disk")" = gpt; then
    parted -s -a optimal "$disk" \
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
    echo -n "$lukspw" | cryptsetup luksFormat "$luksdev" - \
        -h sha512 \
        --iter-time 5000
  fi

  if ! test -e "$luksmap"; then
    echo "$lukspw" | cryptsetup luksOpen "$luksdev" "$(basename "$luksmap")" -
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
    mkfs.xfs "$rootdev"
  fi

  if ! test "$(blkid -o value -s TYPE "$homedev")" = btrfs; then
    mkfs.xfs "$homedev"
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

  echo READY.
''

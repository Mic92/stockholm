#! /bin/sh
set -eux
{
  umount /mnt/nix
  umount /mnt/root
  umount /boot || :
  umount /mnt/boot
  umount /mnt

  coreutils_path=$(set +f; for i in /nix/store/*coreutils*/bin; do :; done; echo $i)
  sed_path=$(set +f; for i in /nix/store/*gnused*/bin; do :; done; echo $i)
  PATH="$coreutils_path:$sed_path"

  export PATH

  mkdir /oldshit

  #fix bug where grub install cant find the /nix/store because its under a bind mount
  if test -e /boot/grub/grub.cfg; then
    sed -i 's,//store,/nix/store,g' /boot/grub/grub.cfg
  fi;

  mv /bin /oldshit/
  mv /newshit/bin /

  # TODO ensure /boot is empty
  # skip boot
  rmdir /newshit/boot

  # skip /dev
  rmdir /newshit/dev

  mv /etc /oldshit/
  mv /newshit/etc /

  # skip /nix (it's already there)
  rmdir /newshit/nix

  # skip /proc
  rmdir /newshit/proc

  # skip /run
  rmdir /newshit/run

  # skip /sys
  rmdir /newshit/sys

  # skip /root
  rmdir /newshit/root

  # skip /tmp
  # TODO rmdir /newshit/tmp

  mv /home /oldshit/
  mv /newshit/home /

  mv /usr /oldshit/
  mv /newshit/usr /

  mv /var /oldshit/
  mv /newshit/var /

  mv /lib /oldshit/
  mv /lib64 /oldshit/
  mv /sbin /oldshit/
  mv /srv /oldshit/
  mv /opt /oldshit/


  mv /newshit /root/  # TODO this one shoult be empty
  mv /oldshit /root/

  sync
}

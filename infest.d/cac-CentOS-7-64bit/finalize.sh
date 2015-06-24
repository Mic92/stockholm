#! /bin/sh
set -eu
{
  umount /mnt2
  umount /mnt/nix
  umount /mnt/boot
  umount /mnt
  umount /boot

  PATH=$(for i in /nix/store/*coreutils*/bin; do :; done; echo $i)
  export PATH

  mkdir /oldshit

  mv /bin /oldshit/
  mv /newshit/bin /

  # TODO ensure /boot is empty
  rmdir /newshit/boot

  # skip /dev
  rmdir /newshit/dev

  mv /etc /oldshit/
  mv /newshit/etc /

  # TODO ensure /home is empty
  rmdir /newshit/home

  # skip /nix (it's already there)
  rmdir /newshit/nix

  # skip /proc
  rmdir /newshit/proc

  # skip /run
  rmdir /newshit/run

  # skip /sys
  rmdir /newshit/sys

  # skip /tmp
  # TODO rmdir /newshit/tmp

  mv /usr /oldshit/
  mv /newshit/usr /

  mv /var /oldshit/
  mv /newshit/var /

  mv /root /oldshit/
  mv /newshit/root /

  mv /lib /oldshit/
  mv /lib64 /oldshit/
  mv /sbin /oldshit/
  mv /mnt2 /oldshit/
  mv /srv /oldshit/
  mv /opt /oldshit/


  mv /newshit /root/  # TODO this one shoult be empty
  mv /oldshit /root/

  sync
}

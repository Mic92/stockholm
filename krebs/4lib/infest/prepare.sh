#! /bin/sh
set -efu

prepare() {(
  if test -e /etc/os-release; then
    . /etc/os-release
    case $ID in
      arch)
        prepare_arch "$@"
        exit
        ;;
      centos)
        case $VERSION_ID in
          6)
            prepare_centos "$@"
            exit
            ;;
          7)
            prepare_centos "$@"
            exit
            ;;
        esac
        ;;
    esac
  elif test -e /etc/centos-release; then
    case $(cat /etc/centos-release) in
      'CentOS release 6.5 (Final)')
        prepare_centos "$@"
        exit
        ;;
    esac
  fi
  echo "$0 prepare: unknown OS" >&2
  exit -1
)}

prepare_arch() {
  type bzip2 2>/dev/null || pacman -S --noconfirm bzip2
  type git   2>/dev/null || pacman -S --noconfirm git
  type rsync 2>/dev/null || pacman -S --noconfirm rsync
  prepare_common
}

prepare_centos() {
  type bzip2 2>/dev/null || yum install -y bzip2
  type git   2>/dev/null || yum install -y git
  type rsync 2>/dev/null || yum install -y rsync
  prepare_common
}

prepare_common() {

  if ! getent group nixbld >/dev/null; then
    groupadd -g 30000 -r nixbld
  fi
  for i in `seq 1 10`; do
    if ! getent passwd nixbld$i 2>/dev/null; then
      useradd \
        -d /var/empty \
        -g 30000 \
        -G 30000 \
        -l \
        -M \
        -s /sbin/nologin \
        -u $(expr 30000 + $i) \
        nixbld$i
    fi
  done

  #
  # mount install directory
  #

  if ! mount | grep -Fq ' on /mnt type '; then
    mkdir -p /newshit
    mount --bind /newshit /mnt
  fi

  if ! mount | grep -Fq ' on /mnt/boot type '; then
    mkdir -p /mnt/boot

    if mount | grep -Fq ' on /boot type '; then
      bootdev=$(mount | grep " on /boot type " | sed 's/ .*//')
      mount $bootdev /mnt/boot
    else
      mount --bind /boot/ /mnt/boot
    fi

  fi

  #
  # prepare install directory
  #

  rootpart=$(mount | grep " on / type" | sed 's/ .*//')

  mkdir -p /mnt/etc/nixos
  mkdir -m 0555 -p /mnt/var/empty

  if ! mount | grep -Fq "$rootpart on /mnt/root type "; then
    mkdir -p /mnt/root
    mount --bind /root /mnt/root
  fi

  #
  # prepare nix store path
  #

  mkdir -v -m 0755 -p /nix
  if ! mount | grep -Fq "$rootpart on /mnt/nix type "; then
    mkdir -p /mnt/nix
    mount --bind /nix /mnt/nix
  fi
}

prepare "$@"

#! /bin/sh
set -efu

prepare() {(
  if test -e /etc/os-release; then
    . /etc/os-release
    case $ID in
      centos)
        case $VERSION_ID in
          7)
            prepare_centos7 "$@"
            exit
            ;;
        esac
        ;;
    esac
  fi
  echo "$0 prepare: unknown OS" >&2
  exit -1
)}

prepare_centos7() {
  type bzip2 2>/dev/null || yum install -y bzip2
  type git   2>/dev/null || yum install -y git
  type rsync 2>/dev/null || yum install -y rsync
  if ! getent group nixbld >/dev/null; then
    groupadd -g 30000 -r nixbld
  fi
  for i in `seq 1 10`; do
    if ! getent passwd nixbld$i 2>/dev/null; then
      useradd \
        -c "CentOS Nix build user $i" \
        -d /var/empty \
        -g 30000 \
        -G 30000 \
        -l \
        -M \
        -s /sbin/nologin \
        -u $(expr 30000 + $i) \
        nixbld$i
      rm -f /var/spool/mail/nixbld$i
    fi
  done

  #
  # mount install directory
  #

  if ! mount | grep -Fq '/dev/mapper/centos-root on /mnt type xfs'; then
    mkdir -p /newshit
    mount --bind /newshit /mnt
  fi

  if ! mount | grep -Fq '/dev/sda1 on /mnt/boot type xfs'; then
    mkdir -p /mnt/boot
    mount /dev/sda1 /mnt/boot
  fi

  mount | grep 'on /mnt\>' >&2

  #
  # prepare install directory
  #

  mkdir -p /mnt/etc/nixos
  mkdir -m 0555 -p /mnt/var/empty

  if ! mount | grep -Fq '/dev/mapper/centos-root on /mnt/root type xfs'; then
    mkdir -p /mnt/root
    mount --bind /root /mnt/root
  fi
}

prepare "$@"

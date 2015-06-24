#! /bin/sh
set -euf

: $nix_url
: $nix_sha256

{
  #
  # prepare host
  #

  type bzip2 2>/dev/null || yum install -y bzip2
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

  # generate fake sudo because
  # sudo: sorry, you must have a tty to run sudo
  mkdir -p bin
  printf '#! /bin/sh\nexec env "$@"\n' > bin/sudo
  chmod +x bin/sudo

  PATH=$PWD/bin:$PATH
  export PATH

  # install nix on host (cf. https://nixos.org/nix/install)
  if ! test -e /root/.nix-profile/etc/profile.d/nix.sh; then
    (
      verify() {
        echo $nix_sha256  $(basename $nix_url) | sha256sum -c
      }
      if ! verify; then
        curl -C - -O "$nix_url"
        verify
      fi
    )
    tar jxf $(basename $nix_url)
    $(basename $nix_url .tar.bz2)/install
  fi

  MANPATH=/var/empty . /root/.nix-profile/etc/profile.d/nix.sh

  if ! type nixos-install 2>/dev/null; then
    nixpkgs_expr='import <nixpkgs> { system = builtins.currentSystem; }'
    nixpkgs_path=$(find /nix/store -mindepth 1 -maxdepth 1 -name *-nixpkgs-* -type d)
    nix-env \
      --arg config "{ nix.package = ($nixpkgs_expr).nix; }" \
      --arg pkgs "$nixpkgs_expr" \
      --arg modulesPath 'throw "no modulesPath"' \
      -f $nixpkgs_path/nixpkgs/nixos/modules/installer/tools/tools.nix \
      -iA config.system.build.nixos-install
  fi

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

  if ! mount | grep -Fq '/dev/mapper/centos-root on /mnt/nix type xfs'; then
    mkdir -p /mnt/nix
    mount --bind /nix /mnt/nix
  fi

  mount | grep 'on /mnt\>' >&2

  #
  # prepare install directory
  #
  # XXX This should be done by (?)
  #   remote_dir=/mnt ./cac pushconfig servername:c731445864-cloudpro-134581046 rmdir

  mkdir -p /mnt/etc/nixos
  mkdir -m 0555 -p /mnt/var/empty

  # add eye candy
  address=$(echo $SSH_CONNECTION | awk '{print$3}')
  echo 'PS1='\''\[\e[1;31m\]\u@'"$address"'\[\e[m\] \[\e[1;32m\]\w\[\e[m\] '\' > .bashrc
}

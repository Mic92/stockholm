#! /bin/sh
set -efu

nix_url=https://nixos.org/releases/nix/nix-1.10/nix-1.10-x86_64-linux.tar.bz2
nix_sha256=504f7a3a85fceffb8766ae5e1005de9e02e489742f5a63cc3e7552120b138bf4

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
      debian)
        case $VERSION_ID in
          7)
            prepare_debian "$@"
            exit
            ;;
          8)
            prepare_debian "$@"
            exit
            ;;
        esac
        ;;
      nixos)
        case $(cat /proc/cmdline) in
          *' root=LABEL=NIXOS_ISO '*)
            prepare_nixos_iso "$@"
            exit
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
  pacman -Sy
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

prepare_debian() {
  apt-get update
  type bzip2 2>/dev/null || apt-get install bzip2
  type git   2>/dev/null || apt-get install git
  type rsync 2>/dev/null || apt-get install rsync
  type curl 2>/dev/null || apt-get install curl
  prepare_common
}

prepare_nixos_iso() {
  mountpoint /mnt

  type git 2>/dev/null || nix-env -iA nixos.git

  mkdir -p /mnt/"$target_path"
  mkdir -p "$target_path"

  if ! mountpoint "$target_path"; then
    mount --rbind /mnt/"$target_path" "$target_path"
  fi

  mkdir -p bin
  rm -f bin/nixos-install
  cp "$(type -p nixos-install)" bin/nixos-install
  sed -i "s@^NIX_PATH=\"[^\"]*\"@NIX_PATH=$target_path@" bin/nixos-install
}

get_nixos_install() {
  echo "installing nixos-install" 2>&1
  c=$(mktemp)

  cat <<EOF > $c
{ fileSystems."/" = {};
    boot.loader.grub.enable = false;
}
EOF
  export NIXOS_CONFIG=$c
  nix-env -i -A config.system.build.nixos-install -f "<nixpkgs/nixos>"
  rm -v $c
}
prepare_common() {(

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

  #
  # install nix
  #

  # install nix on host (cf. https://nixos.org/nix/install)
  if ! test -e /root/.nix-profile/etc/profile.d/nix.sh; then
    (
      verify() {
        printf '%s  %s\n' $nix_sha256  $(basename $nix_url) | sha256sum -c
      }
      if ! verify; then
        curl -C - -O "$nix_url"
        verify
      fi
    )
    nix_src_dir=$(basename $nix_url .tar.bz2)
    tar jxf $nix_src_dir.tar.bz2
    $nix_src_dir/install
  fi

  . /root/.nix-profile/etc/profile.d/nix.sh

  mkdir -p /mnt/"$target_path"
  mkdir -p "$target_path"

  if ! mountpoint "$target_path"; then
    mount --rbind /mnt/"$target_path" "$target_path"
  fi

  get_nixos_install
  mkdir -p bin
  rm -f bin/nixos-install
  cp "$(type -p nixos-install)" bin/nixos-install
  sed -i "s@^NIX_PATH=\"[^\"]*\"@NIX_PATH=$target_path@" bin/nixos-install

  if ! grep -q '^PATH.*#krebs' .bashrc; then
    echo '. /root/.nix-profile/etc/profile.d/nix.sh' >> .bashrc
    echo 'PATH=$HOME/bin:$PATH #krebs' >> .bashrc
  fi
)}

prepare "$@"

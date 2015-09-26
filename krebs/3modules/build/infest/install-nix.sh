#! /bin/sh
set -efu

nix_url=https://nixos.org/releases/nix/nix-1.10/nix-1.10-x86_64-linux.tar.bz2
nix_sha256=504f7a3a85fceffb8766ae5e1005de9e02e489742f5a63cc3e7552120b138bf4

install_nix() {(

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
    mkdir -v -m 0755 -p /nix
    $nix_src_dir/install
  fi

  #TODO: make this general or move to prepare
  if ! mount | grep -Fq '/dev/mapper/centos-root on /mnt/nix type xfs'; then
    mkdir -p /mnt/nix
    mount --bind /nix /mnt/nix
  fi

  . /root/.nix-profile/etc/profile.d/nix.sh

  for i in \
    bash \
    coreutils \
    # This line intentionally left blank.
  do
    if ! nix-env -q $i | grep -q .; then
      nix-env -iA nixpkgs.pkgs.$i
    fi
  done

  # install nixos-install
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
)}

install_nix "$@"

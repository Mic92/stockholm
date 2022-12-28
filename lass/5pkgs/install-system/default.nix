{ pkgs }:
pkgs.writers.writeDashBin "install-system" ''
  set -efux
  SYSTEM=$1
  TARGET=$2
  # format
  if ! (sshn "$TARGET" -- mountpoint /mnt); then
    nix run github:numtide/nixos-remote -- --stop-after-disko --store-paths "$(nix-build --no-out-link -I stockholm="$HOME"/sync/stockholm -I nixos-config="$HOME"/sync/stockholm/lass/1systems/"$SYSTEM"/physical.nix '<nixpkgs/nixos>' -A config.system.build.diskoNoDeps)" /dev/null "$TARGET"
  fi

  # install dependencies
  sshn "$TARGET" << SSH
    nix-channel --update
    nix-env -iA nixos.git
  SSH

  # populate
  $(nix-build --no-out-link "$HOME"/sync/stockholm/lass/krops.nix -A populate --argstr name "$SYSTEM" --argstr target "$TARGET"/mnt/var/src --arg force true)

  # install
  sshn "$TARGET" << SSH
    ln -s /mnt/var/src /var/src
    NIXOS_CONFIG=/var/src/nixos-config nixos-install --no-root-password -I /var/src
    zpool export -fa
  SSH
''

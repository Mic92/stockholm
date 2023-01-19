{ pkgs, ... }: {
  nix.settings.auto-optimise-store = true;

  # TODO check if both are required:
  nix.settings.extra-sandbox-paths = [
    "/etc/protocols"
    pkgs.iana-etc.outPath
  ];
}

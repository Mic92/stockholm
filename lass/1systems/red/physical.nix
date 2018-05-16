{
  imports = [
    ./config.nix
  ];
  boot.isContainer = true;
  networking.useDHCP = false;
  environment.variables.NIX_REMOTE = "daemon";
}

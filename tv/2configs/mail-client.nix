{ pkgs, ... }: {
  environment.systemPackages = [
    pkgs.haskellPackages.much
    pkgs.msmtp
    pkgs.notmuch
    pkgs.pythonPackages.alot
    pkgs.qprint
    pkgs.w3m
  ];
}

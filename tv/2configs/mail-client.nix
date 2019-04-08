{ pkgs, ... }: {
  environment.systemPackages = [
    pkgs.haskellPackages.much
    pkgs.msmtp
    pkgs.notmuch
    pkgs.qprint
    pkgs.w3m
  ];
}

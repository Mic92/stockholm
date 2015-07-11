{ ... }:

{
  nixpkgs.config.packageOverrides = pkgs:
    {
      nano = pkgs.runCommand "empty" {} "mkdir -p $out";
    };

  services.cron.enable = false;
  services.nscd.enable = false;
  services.ntp.enable = false;
}

{ ... }:

{
  nixpkgs.config.packageOverrides = pkgs:
    {
      nano = /var/empty;
    };
}

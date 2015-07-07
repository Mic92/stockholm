{ ... }:

{
  nixpkgs.config.packageOverrides = pkgs:
    {
      nano = pkgs.runCommand "empty" {} "mkdir -p $out";
    };
}

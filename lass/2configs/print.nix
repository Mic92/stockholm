{ pkgs, ... }:
{
  services.printing = {
    enable = true;
    drivers = [
      pkgs.foomatic-filters
      pkgs.gutenprint
    ];
  };
}

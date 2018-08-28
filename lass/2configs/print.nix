{ pkgs, ... }:
{
  services.printing = {
    enable = true;
    drivers = [
      pkgs.foomatic_filters
      pkgs.gutenprint
    ];
  };
}

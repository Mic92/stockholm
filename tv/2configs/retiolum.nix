{ config, lib, ... }:

with lib;

{
  krebs.retiolum = {
    enable = true;
    connectTo = filter (ne config.krebs.build.host.name) [
      "gum"
      "prism"
      "echelon"
      "cd"
      "ire"
    ];
  };
}

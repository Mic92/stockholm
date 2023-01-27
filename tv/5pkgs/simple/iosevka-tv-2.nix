{ pkgs }:

pkgs.iosevka.override {
  # https://typeof.net/Iosevka/customizer
  privateBuildPlan = {
    family = "iosevka tv 2";
    spacing = "term";
    serifs = "sans";
    export-glyph-names = true;
    no-ligation = true;
    no-cv-ss = false;

    variants.inherits = "ss10";

    widths.normal.shape = 600;
    widths.normal.menu = 5;
    widths.normal.css = "normal";
  };
  set = "tv-2";
}

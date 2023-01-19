{ pkgs }:

pkgs.iosevka.override {
  # https://typeof.net/Iosevka/customizer
  privateBuildPlan = {
    family = "iosevka-tv-1";
    spacing = "term";
    serifs = "sans";
    export-glyph-names = true;
    no-ligation = true;
    no-cv-ss = false;

    widths.normal.shape = 600;
    widths.normal.menu = 5;
    widths.normal.css = "normal";
  };
  set = "iosevka-tv-1";
}

with import ./lib;
{ config, pkgs, ... }:

let
  cfg = config.services.xserver;
in

pkgs.stdenv.mkDerivation {
  name = "xserver.conf";

  fontPath = optionalString (cfg.fontPath != null)
    ''FontPath "${toString cfg.fontPath}"'';

  inherit (cfg) config;

  buildCommand =
    ''
      echo 'Section "Files"' >> $out
      echo $fontPath >> $out

      for i in ${toString config.fonts.fonts}; do
        if test "''${i:0:''${#NIX_STORE}}" == "$NIX_STORE"; then
          for j in $(find $i -name fonts.dir); do
            echo "  FontPath \"$(dirname $j)\"" >> $out
          done
        fi
      done

      for i in $(find ${toString cfg.modules} -type d); do
        if test $(echo $i/*.so* | wc -w) -ne 0; then
          echo "  ModulePath \"$i\"" >> $out
        fi
      done

      echo 'EndSection' >> $out

      echo "$config" >> $out
    '';
}

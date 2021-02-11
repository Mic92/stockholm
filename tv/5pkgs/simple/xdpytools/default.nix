{ lib, pkgs }:

let
  install = name: { path }: /* sh */ ''
    (
      mkdir -p $out/bin
      touch $out/bin/${name}
      chmod +x $out/bin/${name}
      exec >$out/bin/${name}

      echo '#! ${pkgs.dash}/bin/dash'
      echo export PATH=${lib.makeBinPath path}
      sed 1d ${./src + "/${name}"}
    )
  '';
in

pkgs.runCommand "xdpytools" {}
  (toString
    (lib.mapAttrsToList install {
      xdpychvt.path = [
        "$out"
        "/run/wrappers/'$LOGNAME'"
        "/run/wrappers"
      ];
      xdpysel.path = [
        "$out"
        pkgs.findutils
        pkgs.jq
      ];
    }))

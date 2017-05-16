{ pkgs, lib, ... }:

let

  scripts = lib.concatStringsSep "," [
    good
    delete
  ];

  mpv = pkgs.concat "mpv" [
    pkgs.mpv
    (pkgs.writeDashBin "mpv" ''
      exec ${pkgs.mpv}/bin/mpv --no-config --script=${scripts} "$@"
    '')
  ];

  moveToDir = key: dir: pkgs.writeText "move-with-${key}.lua" ''
    tmp_dir = "${dir}"

    function move_current_track_${key}()
      track = mp.get_property("path")
      os.execute("mkdir -p '" .. tmp_dir .. "'")
      os.execute("mv '" .. track .. "' '" .. tmp_dir .. "'")
      print("moved '" .. track .. "' to " .. tmp_dir)
    end

    mp.add_key_binding("${key}", "move_current_track_${key}", move_current_track_${key})
  '';

  good = moveToDir "G" "./.good";
  delete = moveToDir "D" "./.graveyard";

  up = moveToDir "U" "./up";
  down = moveToDir "Y" "./down";

in {
  environment.systemPackages = [
    mpv
  ];
}

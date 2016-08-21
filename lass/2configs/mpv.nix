{ pkgs, lib, ... }:

let

  mpv-config = pkgs.writeText "mpv-config" ''
    script=${lib.concatStringsSep "," [
      good
      delete
    ]}
  '';
  mpv = pkgs.writeDashBin "mpv" ''
    exec ${pkgs.mpv}/bin/mpv --no-config --include=${mpv-config} "$@"
  '';

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

  deleteCurrentTrack = pkgs.writeText "delete.lua" ''
    deleted_tmp = "./.graveyard"

    -- Delete the current track by moving it to the `deleted_tmp` location.
    function delete_current_track()
      track = mp.get_property("path")
      os.execute("mkdir -p '" .. deleted_tmp .. "'")
      os.execute("mv '" .. track .. "' '" .. deleted_tmp .. "'")
      print("'" .. track .. "' deleted.")
    end

    mp.add_key_binding("D", "delete_current_track", delete_current_track)
  '';

in {
  krebs.per-user.lass.packages = [
    mpv
  ];
}

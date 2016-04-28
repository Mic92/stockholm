{ pkgs, ... }:

let

  mpv-config = pkgs.writeText "mpv-config" ''
    script=${deleteCurrentFile}
  '';
  mpv = pkgs.writeDashBin "mpv" ''
    exec ${pkgs.mpv}/bin/mpv --no-config --include=${mpv-config} $@
  '';

  deleteCurrentFile = pkgs.writeText "delete.lua" ''
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

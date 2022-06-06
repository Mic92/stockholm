{ pkgs, lib, ... }:

let

  download_subs = pkgs.writers.writePython3 "download_sub" {
    libraries = [ pkgs.python3Packages.subliminal ];
  } ''
    from subliminal import download_best_subtitles, scan_video
    from babelfish import Language
    import sys

    video_filename = sys.argv[1]

    vid = scan_video(video_filename)
    try:
        sub = download_best_subtitles([vid], {Language('eng')})[vid][0]

        filename = '/tmp/' + vid.title + '.srt'

        with open(filename, 'wb+') as file:
            file.write(sub.content)

        print(filename)
    except:  # noqa
        print("/dev/null")
  '';

  autosub = pkgs.writeText "autosub.lua" ''
    -- Requires Subliminal version 1.0 or newer
    -- Make sure to specify your system's Subliminal location below:
    local utils = require 'mp.utils'

    -- Log function: log to both terminal and mpv OSD (On-Screen Display)
    function log(string, secs)
        secs = secs or 2     -- secs defaults to 2 when the secs parameter is absent
        mp.msg.warn(string)          -- This logs to the terminal
        mp.osd_message(string, secs) -- This logs to mpv screen
    end

    function download()
        log('Searching subtitles ...', 10)
        table = { args = {"${download_subs}", mp.get_property('path')} }
        result = utils.subprocess(table)
        if result.error == nil then
            -- remove trailing newline from subtitle filename
            filename = string.gsub(result.stdout, "\n", "")
            log(filename)
            mp.commandv('sub_add', filename)
            log('Subtitles ready!')
        else
            log('Subtitles failed downloading')
        end
    end

    -- Control function: only download if necessary
    function control_download()
        duration = tonumber(mp.get_property('duration'))
        if duration < 900 then
            mp.msg.warn('Video is less than 15 minutes\n', '=> NOT downloading any subtitles')
            return
        end
        -- There does not seem to be any documentation for the 'sub' property,
        -- but it works on both internally encoded as well as external subtitle files!
        -- -> sub = '1' when subtitles are present
        -- -> sub = 'no' when subtitles are not present
        -- -> sub = 'auto' when called before the 'file-loaded' event is triggered
        sub = mp.get_property('sub')
        if sub == '1' then
            mp.msg.warn('Sub track is already present\n', '=> NOT downloading other subtitles')
            return
        end
        mp.msg.warn('No sub track was detected\n', '=> Proceeding to download subtitles:')
        download()
    end

    mp.add_key_binding('S', "download_subs", download)
  '';

  mpvInput = pkgs.writeText "mpv.input" ''
    : script-binding console/enable
  '';

  mpvConfig = pkgs.writeText "mpv.conf" ''
    osd-font-size=20
  '';

  mpv = pkgs.symlinkJoin {
    name = "mpv";
    paths = [
      (pkgs.writeDashBin "mpv" ''
        set -efu
        if [ -n "''${DISPLAY+x}" ]; then
          Y_RES=$(${pkgs.xorg.xrandr}/bin/xrandr |
            ${pkgs.jc}/bin/jc --xrandr |
            ${pkgs.jq}/bin/jq '.screens[0].current_width'
          )
        else
          Y_RES=1000
        fi
        # we need to disable sponsorblock local database because of
        # https://github.com/po5/mpv_sponsorblock/issues/31
        exec ${pkgs.mpv.override {
          scripts = with pkgs.mpvScripts; [
            sponsorblock
            youtube-quality
          ];
        }}/bin/mpv \
         -vo=gpu \
         --no-config \
         --input-conf=${mpvInput} \
         --include=${mpvConfig} \
         --script=${autosub} \
         --ytdl-format="best[height<$Y_RES]" \
         --script-opts=ytdl_hook-ytdl_path=${pkgs.yt-dlp}/bin/yt-dlp \
         --script-opts-append=sponsorblock-local_database=no \
         "$@"
      '')
      pkgs.mpv
    ];
  };

in {
  environment.systemPackages = [
    mpv
  ];
}

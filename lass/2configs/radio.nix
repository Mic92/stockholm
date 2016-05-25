{ config, pkgs, ... }:

with config.krebs.lib;

let
  name = "radio";
  mainUser = config.users.extraUsers.mainUser;
  inherit (config.krebs.lib) genid;

  admin-password = import <secrets/icecast-admin-pw>;
  source-password = import <secrets/icecast-source-pw>;

  add_random = pkgs.writeDashBin "add_random" ''
    mpc add "$(mpc ls | shuf -n1)"
  '';

  skip_track = pkgs.writeDashBin "skip_track" ''
    ${add_random}/bin/add_random
    echo skipping: "$(${print_current}/bin/print_current)"
    ${pkgs.mpc_cli}/bin/mpc -q next
  '';

  print_current = pkgs.writeDashBin "print_current" ''
    echo "$(${pkgs.mpc_cli}/bin/mpc current -f %file%) \
    $(${pkgs.mpc_cli}/bin/mpc current -f %file% \
      | ${pkgs.gnused}/bin/sed 's@.*\(.\{11\}\)\.ogg@http://www.youtube.com/watch?v=\1@')"
  '';

in {
  users.users = {
    "${name}" = rec {
      inherit name;
      group = name;
      uid = genid name;
      description = "radio manager";
      home = "/home/${name}";
      useDefaultShell = true;
      createHome = true;
      openssh.authorizedKeys.keys = [
        config.krebs.users.lass.pubkey
      ];
    };
  };

  users.groups = {
    "radio" = {};
  };

  krebs.per-user.${name}.packages = with pkgs; [
    add_random
    skip_track
    print_current
    ncmpcpp
    mpc_cli
    tmux
  ];

  security.sudo.extraConfig = ''
    ${mainUser.name} ALL=(${name}) NOPASSWD: ALL
  '';

  services.mpd = {
    enable = true;
    group = "radio";
    musicDirectory = "/home/radio/the_playlist/music";
    extraConfig = ''
      audio_output {
          type        "shout"
          encoding    "ogg"
          name        "my cool stream"
          host        "localhost"
          port        "8000"
          mount       "/radio.ogg"

      # This is the source password in icecast.xml
          password    "${source-password}"

      # Set either quality or bit rate
      #   quality     "5.0"
          bitrate     "128"

          format      "44100:16:1"

      # Optional Parameters
          user        "source"
      #   description "here is my long description"
      #   genre       "jazz"
      } # end of audio_output

    '';
  };

  services.icecast = {
    enable = true;
    hostname =  "config.krebs.build.host.name";
    admin.password = admin-password;
    extraConf = ''
      <authentication>
        <source-password>${source-password}</source-password>
      </authentication>
    '';
  };

  krebs.iptables = {
    tables = {
      filter.INPUT.rules = [
        { predicate = "-p tcp --dport 8000"; target = "ACCEPT"; }
      ];
    };
  };

  systemd.timers.radio = {
    description = "radio autoadder timer";
    wantedBy = [ "timers.target" ];

    timerConfig = {
      OnCalendar = "*:*";
    };
  };

  systemd.services.radio = let
    autoAdd = pkgs.writeDash "autoAdd" ''
      LIMIT=$1 #in secconds

      timeLeft () {
        playlistDuration=$(mpc --format '%time%' playlist | awk -F ':' 'BEGIN{t=0} {t+=$1*60+$2} END{print t}')
        currentTime=$(mpc status | awk '/^\[playing\]/ { sub(/\/.+/,"",$3); split($3,a,/:/); print a[1]*60+a[2] }')
        expr ''${playlistDuration:-0} - ''${currentTime:-0}
      }

      if test $(timeLeft) -le $LIMIT; then
        ${add_random}/bin/add_random
      fi
    '';
  in {
    description = "radio playlist autoadder";
    after = [ "network.target" ];

    path = with pkgs; [
      gawk
      mpc_cli
    ];

    restartIfChanged = true;

    serviceConfig = {
      Restart = "always";
      ExecStart = "${autoAdd} 100";
    };
  };

  krebs.Reaktor = {
    enable = true;
    nickname = "the_playlist|r";
    channels = [ "#the_playlist" ];
    extraEnviron = {
      REAKTOR_HOST = "irc.freenode.org";
    };
    plugins = with pkgs.ReaktorPlugins; [
      (buildSimpleReaktorPlugin "skip" {
        script = "${skip_track}/bin/skip_track";
        pattern = "^skip$";
      })
      (buildSimpleReaktorPlugin "current" {
        script = "${print_current}/bin/print_current";
        pattern = "^current$";
      })
    ];
  };
  krebs.nginx.servers."lassul.us".locations = let
    html = pkgs.writeText "index.html" ''
      <!DOCTYPE html>
      <html lang="en">
        <head>
          <meta charset="utf-8">
          <title>lassulus playlist</title>
        </head>
        <body>
          <div style="display:inline-block;margin:0px;padding:0px;overflow:hidden">
            <iframe src="https://kiwiirc.com/client/irc.freenode.org/?nick=kiwi_test|?&theme=cli#the_playlist" frameborder="0" style="overflow:hidden;overflow-x:hidden;overflow-y:hidden;height:95%;width:100%;position:absolute;top:0px;left:0px;right:0px;bottom:0px" height="95%" width="100%"></iframe>
          </div>
          <div style="position:absolute;bottom:1px;display:inline-block;background-color:red;">
            <audio controls autoplay="autoplay"><source src="http://lassul.us:8000/radio.ogg" type="audio/ogg">Your browser does not support the audio element.</audio>
          </div>
          <!-- page content -->
        </body>
      </html>
    '';
  in [
    (nameValuePair "/the_playlist" ''
      default_type "text/html";
      alias ${html};
    '')
  ];
}

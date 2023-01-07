{ config, pkgs, lib, ... }:

let
  name = "radio";

  music_dir = "/var/music";

  skip_track = pkgs.writers.writeBashBin "skip_track" ''
    set -eu

    # TODO come up with new rating, without moving files
    # current_track=$(${pkgs.curl}/bin/curl -fSs http://localhost:8002/current | ${pkgs.jq}/bin/jq -r .filename)
    # track_infos=$(${print_current}/bin/print_current)
    # skip_count=$(${pkgs.attr}/bin/getfattr -n user.skip_count --only-values "$current_track" || echo 0)
    # if [[ "$current_track" =~ .*/the_playlist/music/.* ]] && [ "$skip_count" -le 2 ]; then
    #   skip_count=$((skip_count+1))
    #   ${pkgs.attr}/bin/setfattr -n user.skip_count -v "$skip_count" "$current_track"
    #   echo skipping: "$track_infos" skip_count: "$skip_count"
    # else
    #   mkdir -p "$music_dir"/the_playlist/.graveyard/
    #   mv "$current_track" "$music_dir"/the_playlist/.graveyard/
    #   echo killing: "$track_infos"
    # fi
    ${pkgs.curl}/bin/curl -fSs -X POST http://localhost:8002/skip |
      ${pkgs.jq}/bin/jq -r '.filename'
  '';

  good_track = pkgs.writeBashBin "good_track" ''
    set -eu

    current_track=$(${pkgs.curl}/bin/curl -fSs http://localhost:8002/current | ${pkgs.jq}/bin/jq -r .filename)
    track_infos=$(${print_current}/bin/print_current)
    # TODO come up with new rating, without moving files
    # if [[ "$current_track" =~ .*/the_playlist/music/.* ]]; then
    #   ${pkgs.attr}/bin/setfattr -n user.skip_count -v 0 "$current_track"
    # else
    #   mv "$current_track" "$music_dir"/the_playlist/music/ || :
    # fi
    echo good: "$track_infos"
  '';

  print_current = pkgs.writeDashBin "print_current" ''
    file=$(${pkgs.curl}/bin/curl -fSs http://localhost:8002/current |
      ${pkgs.jq}/bin/jq -r '.filename' |
      ${pkgs.gnused}/bin/sed 's,^${music_dir},,'
    )
    link=$(${pkgs.curl}/bin/curl http://localhost:8002/current |
      ${pkgs.jq}/bin/jq -r '.filename' |
      ${pkgs.gnused}/bin/sed 's@.*\(.\{11\}\)\.ogg@https://youtu.be/\1@'
    )
    echo "$file": "$link"
  '';

  set_irc_topic = pkgs.writeDash "set_irc_topic" ''
    ${pkgs.curl}/bin/curl -fsS --unix-socket /home/radio/reaktor.sock http://z/ \
      -H content-type:application/json \
      -d "$(${pkgs.jq}/bin/jq -n \
        --arg text "$1" '{
          command:"TOPIC",
          params:["#the_playlist",$text]
        }'
      )"
  '';

  write_to_irc = pkgs.writeDash "write_to_irc" ''
    ${pkgs.curl}/bin/curl -fsSv --unix-socket /home/radio/reaktor.sock http://z/ \
      -H content-type:application/json \
      -d "$(${pkgs.jq}/bin/jq -n \
        --arg text "$1" '{
          command:"PRIVMSG",
          params:["#the_playlist",$text]
        }'
      )"
  '';

in {
  imports = [
    ./news.nix
    ./weather.nix
  ];

  users.users = {
    "${name}" = rec {
      inherit name;
      createHome = lib.mkForce false;
      group = name;
      uid = pkgs.stockholm.lib.genid_uint31 name;
      description = "radio manager";
      home = "/home/${name}";
      useDefaultShell = true;
      openssh.authorizedKeys.keys = with config.krebs.users; [
        lass.pubkey
      ];
    };
  };

  users.groups = {
    "radio" = {};
  };

  krebs.per-user.${name}.packages = with pkgs; [
    good_track
    skip_track
    print_current
  ];

  services.liquidsoap.streams.radio = ./radio.liq;
  systemd.services.radio = {
    environment = {
      RADIO_PORT = "8002";
      HOOK_TRACK_CHANGE = pkgs.writers.writeDash "on_change" ''
        set -xefu
        LIMIT=1000 #how many tracks to keep in the history
        HISTORY_FILE=/var/lib/radio/recent

        listeners=$(${pkgs.curl}/bin/curl -fSs http://localhost:8000/status-json.xsl |
          ${pkgs.jq}/bin/jq '[.icestats.source[].listeners] | add' || echo 0)
        echo "$(${pkgs.coreutils}/bin/date -Is)" "$filename" | ${pkgs.coreutils}/bin/tee -a "$HISTORY_FILE"
        echo "$(${pkgs.coreutils}/bin/tail -$LIMIT "$HISTORY_FILE")" > "$HISTORY_FILE"
        ${set_irc_topic} "playing: $filename listeners: $listeners"
      '';
      MUSIC = "${music_dir}/the_playlist";
      ICECAST_HOST = "localhost";
    };
    path = [
      pkgs.yt-dlp
    ];
    serviceConfig.User = lib.mkForce "radio";
  };

  services.icecast = {
    enable = true;
    hostname = "radio.lassul.us";
    admin.password = "hackme";
    extraConf = ''
      <authentication>
       <source-password>hackme</source-password>
      </authentication>
    '';
  };

  krebs.iptables = {
    tables = {
      filter.INPUT.rules = [
        { predicate = "-p tcp --dport 8000"; target = "ACCEPT"; }
        { predicate = "-i retiolum -p tcp --dport 8001"; target = "ACCEPT"; }
      ];
    };
  };

  # allow reaktor2 to modify files
  systemd.services."reaktor2-the_playlist".serviceConfig.DynamicUser = lib.mkForce false;

  krebs.reaktor2.the_playlist = {
    hostname = "irc.hackint.org";
    port = "6697";
    useTLS = true;
    nick = "the_playlist";
    username = "radio";
    API.listen = "unix:/home/radio/reaktor.sock";
    plugins = [
      {
        plugin = "register";
        config = {
          channels = [
            "#the_playlist"
            "#krebs"
          ];
        };
      }
      {
        plugin = "system";
        config = {
          workdir = config.krebs.reaktor2.the_playlist.stateDir;
          hooks.PRIVMSG = [
            {
              activate = "match";
              pattern = "^(?:.*\\s)?\\s*the_playlist:\\s*([0-9A-Za-z._][0-9A-Za-z._-]*)(?:\\s+(.*\\S))?\\s*$";
              command = 1;
              arguments = [2];
              commands = {
                skip.filename = "${skip_track}/bin/skip_track";
                next.filename = "${skip_track}/bin/skip_track";
                bad.filename = "${skip_track}/bin/skip_track";

                good.filename = "${good_track}/bin/good_track";
                nice.filename = "${good_track}/bin/good_track";
                like.filename = "${good_track}/bin/good_track";

                current.filename = "${print_current}/bin/print_current";
                wish.filename = pkgs.writeDash "wish" ''
                  echo "youtube-dl:$1" | ${pkgs.curl}/bin/curl -fSs http://localhost:8002/wish -d @- > /dev/null
                '';
                wishlist.filename = pkgs.writeDash "wishlist" ''
                  ${pkgs.curl}/bin/curl -fSs http://localhost:8002/wish | ${pkgs.jq}/bin/jq -r '.[]'
                '';
                suggest.filename = pkgs.writeDash "suggest" ''
                  echo "$@" >> playlist_suggest
                '';
              };
            }
          ];
        };
      }
    ];
  };

  krebs.htgen.radio = {
    port = 8001;
    user = {
      name = "radio";
    };
    scriptFile = pkgs.writeDash "radio" ''
      case "$Method $Request_URI" in
        "POST /skip")
          printf 'HTTP/1.1 200 OK\r\n'
          printf 'Connection: close\r\n'
          printf '\r\n'
          msg=$(${skip_track}/bin/skip_track)
          ${write_to_irc} "$msg"
          echo "$msg"
          exit
        ;;
        "POST /good")
          printf 'HTTP/1.1 200 OK\r\n'
          printf 'Connection: close\r\n'
          printf '\r\n'
          msg=$(${good_track}/bin/good_track)
          ${write_to_irc} "$msg"
          echo "$msg"
          exit
        ;;
      esac
    '';
  };

  services.nginx = {
    enable = true;
    virtualHosts."radio.lassul.us" = {
      forceSSL = true;
      enableACME = true;
      locations."/".extraConfig = ''
        proxy_set_header Host $host;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Host $host;
        proxy_set_header X-Forwarded-Server $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_pass http://localhost:8000;
      '';
      locations."= /recent".extraConfig = ''
        default_type "text/plain";
        alias /var/lib/radio/recent;
      '';
      locations."= /current".extraConfig = ''
        proxy_pass http://localhost:8002;
      '';
      locations."= /skip".extraConfig = ''
        proxy_pass http://localhost:8001;
      '';
      locations."= /good".extraConfig = ''
        proxy_pass http://localhost:8001;
      '';
      locations."= /radio.sh".alias = pkgs.writeScript "radio.sh" ''
        #!/bin/sh
        trap 'exit 0' EXIT
        while sleep 1; do
          mpv \
            --cache-secs=0 --demuxer-readahead-secs=0 --untimed --cache-pause=no \
            'http://lassul.us:8000/radio.ogg'
        done
      '';
      locations."= /controls".extraConfig = ''
        default_type "text/html";
        alias ${./controls.html};
      '';
      extraConfig = ''
        add_header 'Access-Control-Allow-Origin' '*';
        add_header 'Access-Control-Allow-Methods' 'GET, POST, OPTIONS';
      '';
    };
    virtualHosts."lassul.us".locations."= /the_playlist".extraConfig = let
      html = pkgs.writeText "index.html" ''
        <!DOCTYPE html>
        <html lang="en">
          <head>
            <meta charset="utf-8">
            <title>lassulus playlist</title>
          </head>
          <body>
            <div style="display:inline-block;margin:0px;padding:0px;overflow:hidden">
              <iframe src="https://kiwiirc.com/client/irc.hackint.org/?nick=kiwi_test|?&theme=cli#the_playlist" frameborder="0" style="overflow:hidden;overflow-x:hidden;overflow-y:hidden;height:95%;width:100%;position:absolute;top:0px;left:0px;right:0px;bottom:0px" height="95%" width="100%"></iframe>
            </div>
            <div style="position:absolute;bottom:1px;display:inline-block;background-color:red;">
              <audio controls autoplay="autoplay"><source src="http://lassul.us:8000/radio.ogg" type="audio/ogg">Your browser does not support the audio element.</audio>
            </div>
            <!-- page content -->
          </body>
        </html>
      '';
    in ''
      default_type "text/html";
      alias ${html};
    '';
  };
  services.syncthing.declarative.folders."the_playlist" = {
    path = "/var/music/the_playlist";
    devices = [ "mors" "phone" "prism" "omo" ];
  };
  krebs.acl."/var/music/the_playlist"."u:syncthing:X".parents = true;
  krebs.acl."/var/music/the_playlist"."u:syncthing:rwX" = {};
  krebs.acl."/var/music/the_playlist"."u:radio:rwX" = {};
}

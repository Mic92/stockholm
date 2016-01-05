{ config, lib, pkgs, ... }:
with lib;
let
  out = {
    options.krebs.backup = api;
    config = mkIf cfg.enable imp;
  };

  cfg = config.krebs.backup;

  api = {
    enable = mkEnableOption "krebs.backup" // { default = true; };
    plans = mkOption {
      default = {};
      type = types.attrsOf (types.submodule ({
        # TODO enable = mkEnableOption "TODO" // { default = true; };
        options = {
          method = mkOption {
            type = types.enum ["pull" "push"];
          };
          name = mkOption {
            type = types.str;
          };
          src = mkOption {
            type = types.krebs.file-location;
          };
          dst = mkOption {
            type = types.krebs.file-location;
          };
          startAt = mkOption {
            default = "hourly";
            type = types.str; # TODO systemd.time(7)'s calendar event
          };
          snapshots = mkOption {
            default = {
              hourly   = { format = "%Y-%m-%dT%H"; retain =  4; };
              daily    = { format = "%Y-%m-%d";    retain =  7; };
              weekly   = { format = "%YW%W";       retain =  4; };
              monthly  = { format = "%Y-%m";       retain = 12; };
              yearly   = { format = "%Y";                       };
            };
            type = types.attrsOf (types.submodule {
              options = {
                format = mkOption {
                  type = types.str; # TODO date's +FORMAT
                };
                retain = mkOption {
                  type = types.nullOr types.int;
                  default = null; # null = retain all snapshots
                };
              };
            });
          };
        };
      }));
    };
  };

  imp = {
    users.groups.backup.gid = genid "backup";
    users.users = {}
      // {
        root.openssh.authorizedKeys.keys =
          map (plan: plan.dst.host.ssh.pubkey)
              (filter isPullSrc (attrValues cfg.plans))
          ++
          map (plan: plan.src.host.ssh.pubkey)
              (filter isPushDst (attrValues cfg.plans))
          ;
      }
      ;
    systemd.services =
      flip mapAttrs' (filterAttrs (_:isPullDst) cfg.plans) (name: plan: {
        name = "backup.${name}.pull";
        value = makePullService plan;
      })
      //
      flip mapAttrs' (filterAttrs (_:isPushSrc) cfg.plans) (name: plan: {
        name = "backup.${name}.push";
        value = makePushService plan;
      })
      ;
  };

  isPushSrc = plan:
    plan.method == "push" &&
    plan.src.host.name == config.krebs.build.host.name;

  isPullSrc = plan:
    plan.method == "pull" &&
    plan.src.host.name == config.krebs.build.host.name;

  isPushDst = plan:
    plan.method == "push" &&
    plan.dst.host.name == config.krebs.build.host.name;

  isPullDst = plan:
    plan.method == "pull" &&
    plan.dst.host.name == config.krebs.build.host.name;

  # TODO push destination needs this in the dst.user's PATH
  service-path = [
    pkgs.coreutils
    pkgs.gnused
    pkgs.openssh
    pkgs.rsync
    pkgs.utillinux
  ];

  # TODO if there is plan.user, then use its privkey
  makePushService = plan: assert isPushSrc plan; {
    path = service-path;
    serviceConfig = {
      ExecStart = push plan;
      Type = "oneshot";
    };
    startAt = plan.startAt;
  };

  makePullService = plan: assert isPullDst plan; {
    path = service-path;
    serviceConfig = {
      ExecStart = pull plan;
      Type = "oneshot";
    };
    startAt = plan.startAt;
  };

  push = plan: let
    # We use writeDashBin and return the absolute path so systemd will produce
    # nice names in the log, i.e. without the Nix store hash.
    out = "${main}/bin/${main.name}";

    main = writeDashBin "backup.${plan.name}.push" ''
      set -efu
      dst=${shell.escape plan.dst.path}

      mkdir -m 0700 -p "$dst"
      exec flock -n "$dst" ${critical-section}
    '';

    critical-section = writeDash "backup.${plan.name}.push.critical-section" ''
      # TODO check if there is a previous
      set -efu
      identity=${shell.escape plan.src.host.ssh.privkey.path}
      src=${shell.escape plan.src.path}
      dst_target=${shell.escape "root@${getFQDN plan.dst.host}"}
      dst_path=${shell.escape plan.dst.path}
      dst=$dst_target:$dst_path

      # Export NOW so runtime of rsync doesn't influence snapshot naming.
      export NOW
      NOW=$(date +%s)

      echo >&2 "update snapshot: current; $src -> $dst"
      rsync >&2 \
          -aAXF --delete \
          -e "ssh -F /dev/null -i $identity" \
          --rsync-path ${shell.escape
            "mkdir -m 0700 -p ${shell.escape plan.dst.path} && rsync"} \
          --link-dest="$dst_path/current" \
          "$src/" \
          "$dst/.partial"

      exec ssh -F /dev/null \
          -i "$identity" \
          "$dst_target" \
          -T \
          env NOW="$NOW" /bin/sh < ${remote-snapshot}
      EOF
    '';

    remote-snapshot = writeDash "backup.${plan.name}.push.remote-snapshot" ''
      set -efu
      dst=${shell.escape plan.dst.path}

      if test -e "$dst/current"; then
        mv "$dst/current" "$dst/.previous"
      fi
      mv "$dst/.partial" "$dst/current"
      rm -fR "$dst/.previous"
      echo >&2

      (${(take-snapshots plan).text})
    '';

  in out;

  # TODO admit plan.dst.user and its ssh identity
  pull = plan: let
    # We use writeDashBin and return the absolute path so systemd will produce
    # nice names in the log, i.e. without the Nix store hash.
    out = "${main}/bin/${main.name}";

    main = writeDashBin "backup.${plan.name}.pull" ''
      set -efu
      dst=${shell.escape plan.dst.path}

      mkdir -m 0700 -p "$dst"
      exec flock -n "$dst" ${critical-section}
    '';

    critical-section = writeDash "backup.${plan.name}.pull.critical-section" ''
      # TODO check if there is a previous
      set -efu
      identity=${shell.escape plan.dst.host.ssh.privkey.path}
      src=${shell.escape "root@${getFQDN plan.src.host}:${plan.src.path}"}
      dst=${shell.escape plan.dst.path}

      # Export NOW so runtime of rsync doesn't influence snapshot naming.
      export NOW
      NOW=$(date +%s)

      echo >&2 "update snapshot: current; $dst <- $src"
      mkdir -m 0700 -p ${shell.escape plan.dst.path}
      rsync >&2 \
          -aAXF --delete \
          -e "ssh -F /dev/null -i $identity" \
          --link-dest="$dst/current" \
          "$src/" \
          "$dst/.partial"
      mv "$dst/current" "$dst/.previous"
      mv "$dst/.partial" "$dst/current"
      rm -fR "$dst/.previous"
      echo >&2

      exec ${take-snapshots plan}
    '';
  in out;

  take-snapshots = plan: writeDash "backup.${plan.name}.take-snapshots" ''
    set -efu
    NOW=''${NOW-$(date +%s)}
    dst=${shell.escape plan.dst.path}

    snapshot() {(
      : $ns $format $retain
      name=$(date --date="@$NOW" +"$format")
      if ! test -e "$dst/$ns/$name"; then
        echo >&2 "create snapshot: $ns/$name"
        mkdir -m 0700 -p "$dst/$ns"
        rsync >&2 \
            -aAXF --delete \
            --link-dest="$dst/current" \
            "$dst/current/" \
            "$dst/$ns/.partial.$name"
        mv "$dst/$ns/.partial.$name" "$dst/$ns/$name"
        echo >&2
      fi
      case $retain in
        ([0-9]*)
          delete_from=$(($retain + 1))
          ls -r "$dst/$ns" \
            | sed -n "$delete_from,\$p" \
            | while read old_name; do
                echo >&2 "delete snapshot: $ns/$old_name"
                rm -fR "$dst/$ns/$old_name"
              done
          ;;
        (ALL)
          :
          ;;
      esac
    )}

    ${concatStringsSep "\n" (mapAttrsToList (ns: { format, retain ? null, ... }:
      toString (map shell.escape [
        "ns=${ns}"
        "format=${format}"
        "retain=${if retain == null then "ALL" else toString retain}"
        "snapshot"
      ]))
      plan.snapshots)}
  '';

  # TODO getFQDN: admit hosts in other domains
  getFQDN = host: "${host.name}.${config.krebs.search-domain}";

  writeDash = name: text: pkgs.writeScript name ''
    #! ${pkgs.dash}/bin/dash
    ${text}
  '';

  writeDashBin = name: text: pkgs.writeTextFile {
    executable = true;
    destination = "/bin/${name}";
    name = name;
    text = ''
      #! ${pkgs.dash}/bin/dash
      ${text}
    '';
  };

in out
# TODO ionice
# TODO mail on failed push, pull
# TODO mail on missing push
# TODO don't cancel plans on activation
#   also, don't hang while deploying at:
#   starting the following units: backup.wu-home-xu.push.service, backup.wu-home-xu.push.timer
# TODO make sure /bku is properly mounted
# TODO make sure that secure hosts cannot backup to insecure ones
# TODO optionally only backup when src and dst are near enough :)
# TODO try using btrfs for snapshots (configurable)
# TODO warn if partial snapshots are found
# TODO warn if unknown stuff is found in dst path

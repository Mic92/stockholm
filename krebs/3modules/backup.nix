{ config, lib, pkgs, ... }:
with import <stockholm/lib>;
let
  out = {
    options.krebs.backup = api;
    config = lib.mkIf cfg.enable imp;
  };

  cfg = config.krebs.backup;

  api = {
    enable = mkEnableOption "krebs.backup" // { default = true; };
    plans = mkOption {
      default = {};
      type = types.attrsOf (types.submodule ({ config, ... }: {
        options = {
          enable = mkEnableOption "krebs.backup.${config._module.args.name}" // {
            default = true;
          };
          method = mkOption {
            type = types.enum ["pull" "push"];
          };
          name = mkOption {
            type = types.str;
            default = config._module.args.name;
            defaultText = "‹name›";
          };
          src = mkOption {
            type = types.krebs.file-location;
          };
          dst = mkOption {
            type = types.krebs.file-location;
          };
          startAt = mkOption {
            default = "hourly";
            type = with types; nullOr str; # TODO systemd.time(7)'s calendar event
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
          timerConfig = mkOption {
            type = with types; attrsOf str;
            default = optionalAttrs (config.startAt != null) {
              OnCalendar = config.startAt;
            };
          };
        };
      }));
    };
  };

  imp = {
    krebs.on-failure.plans =
      listToAttrs (map (plan: nameValuePair "backup.${plan.name}" {
      }) (filter (plan: build-host-is "pull" "dst" plan ||
                        build-host-is "push" "src" plan)
                 enabled-plans));

    systemd.services =
      listToAttrs (map (plan: nameValuePair "backup.${plan.name}" {
        # TODO if there is plan.user, then use its privkey
        # TODO push destination users need a similar path
        path = with pkgs; [
          coreutils
          gnused
          openssh
          rsync
          util-linux
        ];
        restartIfChanged = false;
        serviceConfig = rec {
          ExecStart = start plan;
          SyslogIdentifier = ExecStart.name;
          Type = "oneshot";
        };
      }) (filter (plan: build-host-is "pull" "dst" plan ||
                        build-host-is "push" "src" plan)
                 enabled-plans));

    systemd.timers =
      listToAttrs (map (plan: nameValuePair "backup.${plan.name}" {
        wantedBy = [ "timers.target" ];
        timerConfig = plan.timerConfig;
      }) (filter (plan: plan.timerConfig != {} && (
                        build-host-is "pull" "dst" plan ||
                        build-host-is "push" "src" plan))
                 enabled-plans));

    users.groups.backup.gid = genid "backup";
    users.users.root.openssh.authorizedKeys.keys =
      map (plan: getAttr plan.method {
        push = plan.src.host.ssh.pubkey;
        pull = plan.dst.host.ssh.pubkey;
      }) (filter (plan: build-host-is "pull" "src" plan ||
                        build-host-is "push" "dst" plan)
                 enabled-plans);
  };

  enabled-plans = filter (getAttr "enable") (attrValues cfg.plans);

  build-host-is = method: side: plan:
    plan.method == method &&
    config.krebs.build.host.name == plan.${side}.host.name;

  start = plan: let
    login-name = "root";
    identity = local.host.ssh.privkey.path;
    ssh = "ssh -i ${shell.escape identity}";
    local = getAttr plan.method {
      push = plan.src // { rsync = src-rsync; };
      pull = plan.dst // { rsync = dst-rsync; };
    };
    remote = getAttr plan.method {
      push = plan.dst // { rsync = dst-rsync; };
      pull = plan.src // { rsync = src-rsync; };
    };
    src-rsync = "rsync";
    dst-rsync = concatStringsSep " && " [
      "stat ${shell.escape plan.dst.path} >/dev/null"
      "mkdir -m 0700 -p ${shell.escape plan.dst.path}/current"
      "flock -n ${shell.escape plan.dst.path} rsync"
    ];
  in pkgs.writeBash "backup.${plan.name}" ''
    set -efu
    start_date=$(date +%s)
    ssh_target=${shell.escape login-name}@$(${fastest-address remote.host})
    ${getAttr plan.method {
      push = ''
        rsync_src=${shell.escape plan.src.path}
        rsync_dst=$ssh_target:${shell.escape plan.dst.path}
        echo >&2 "update snapshot current; $rsync_src -> $rsync_dst"
      '';
      pull = ''
        rsync_src=$ssh_target:${shell.escape plan.src.path}
        rsync_dst=${shell.escape plan.dst.path}
        echo >&2 "update snapshot current; $rsync_dst <- $rsync_src"
      '';
    }}
    # In `dst-rsync`'s `mkdir m 0700 -p` above, we care only about permission
    # of the deepest directory:
    # shellcheck disable=SC2174
    ${local.rsync} >&2 \
        -aAX --delete \
        --filter='dir-merge /.backup-filter' \
        --rsh=${shell.escape ssh} \
        --rsync-path=${shell.escape remote.rsync} \
        --link-dest=${shell.escape plan.dst.path}/current \
        "$rsync_src/" \
        "$rsync_dst/.partial"

    dst_exec() {
      ${getAttr plan.method {
        push = ''exec ${ssh} "$ssh_target" -T "exec$(printf ' %q' "$@")"'';
        pull = ''exec "$@"'';
      }}
    }
    dst_exec env \
        start_date="$start_date" \
        flock -n ${shell.escape plan.dst.path} \
        /bin/sh < ${toFile "backup.${plan.name}.take-snapshots" ''
      set -efu
      : $start_date

      dst_path=${shell.escape plan.dst.path}

      mv "$dst_path/current" "$dst_path/.previous"
      mv "$dst_path/.partial" "$dst_path/current"
      rm -fR "$dst_path/.previous"
      echo >&2

      snapshot() {(
        : $ns $format $retain
        name=$(date --date="@$start_date" +"$format")
        if ! test -e "$dst_path/$ns/$name"; then
          echo >&2 "create snapshot: $ns/$name"
          mkdir -m 0700 -p "$dst_path/$ns"
          rsync >&2 \
              -aAX --delete \
              --filter='dir-merge /.backup-filter' \
              --link-dest="$dst_path/current" \
              "$dst_path/current/" \
              "$dst_path/$ns/.partial.$name"
          mv "$dst_path/$ns/.partial.$name" "$dst_path/$ns/$name"
          echo >&2
        fi
        case $retain in
          ([0-9]*)
            delete_from=$(($retain + 1))
            ls -r "$dst_path/$ns" \
              | sed -n "$delete_from,\$p" \
              | while read old_name; do
                  echo >&2 "delete snapshot: $ns/$old_name"
                  rm -fR "$dst_path/$ns/$old_name"
                done
            ;;
          (ALL)
            :
            ;;
        esac
      )}

      ${concatStringsSep "\n" (mapAttrsToList (ns: { format, retain, ... }:
        toString (map shell.escape [
          "ns=${ns}"
          "format=${format}"
          "retain=${if retain == null then "ALL" else toString retain}"
          "snapshot"
        ]))
        plan.snapshots)}
    ''}
  '';

  # XXX Is one ping enough to determine fastest address?
  fastest-address = host: ''
    { ${pkgs.fping}/bin/fping </dev/null -a -e \
        ${concatMapStringsSep " " shell.escape
          (mapAttrsToList (_: net: head net.aliases) host.nets)} \
      | ${pkgs.gnused}/bin/sed -r 's/^(\S+) \(([0-9.]+) ms\)$/\2\t\1/' \
      | ${pkgs.coreutils}/bin/sort -n \
      | ${pkgs.coreutils}/bin/cut -f2 \
      | ${pkgs.coreutils}/bin/head -n 1
    }
  '';

in out
# TODO ionice
# TODO mail on missing push
# TODO don't cancel plans on activation
#   also, don't hang while deploying at:
#   starting the following units: backup.wu-home-xu.push.service, backup.wu-home-xu.push.timer
# TODO make sure that secure hosts cannot backup to insecure ones
# TODO optionally only backup when src and dst are near enough :)
# TODO try using btrfs for snapshots (configurable)
# TODO warn if partial snapshots are found
# TODO warn if unknown stuff is found in dst path

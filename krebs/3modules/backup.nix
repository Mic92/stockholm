{ config, lib, pkgs, ... }:
with config.krebs.lib;
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
          enable = mkEnableOption "krebs.backup.${config.name}" // {
            default = true;
          };
          method = mkOption {
            type = types.enum ["pull" "push"];
          };
          name = mkOption {
            type = types.str;
            default = config._module.args.name;
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
        };
      }));
    };
  };

  imp = {
    systemd.services =
      listToAttrs (map (plan: nameValuePair "backup.${plan.name}" {
        # TODO if there is plan.user, then use its privkey
        # TODO push destination users need a similar path
        path = with pkgs; [
          coreutils
          gnused
          openssh
          rsync
          utillinux
        ];
        serviceConfig = rec {
          ExecStart = start plan;
          SyslogIdentifier = ExecStart.name;
          Type = "oneshot";
        };
        startAt = mkIf (plan.startAt != null) plan.startAt;
      }) (filter (plan: build-host-is "pull" "dst" plan ||
                        build-host-is "push" "src" plan)
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

  start = plan: pkgs.writeDash "backup.${plan.name}" ''
    set -efu
    ${getAttr plan.method {
      push = ''
        identity=${shell.escape plan.src.host.ssh.privkey.path}
        src_path=${shell.escape plan.src.path}
        src=$src_path
        dst_user=root
        dst_host=$(${fastest-address plan.dst.host})
        dst_port=$(${network-ssh-port plan.dst.host "$dst_host"})
        dst_path=${shell.escape plan.dst.path}
        dst=$dst_user@$dst_host:$dst_path
        echo "update snapshot: current; $src -> $dst" >&2
        dst_shell() {
          exec ssh -F /dev/null \
              -i "$identity" \
              ''${dst_port:+-p $dst_port} \
              "$dst_user@$dst_host" \
              -T "$with_dst_path_lock_script"
        }
        rsh="ssh -F /dev/null -i $identity ''${dst_port:+-p $dst_port}"
        local_rsync() {
          rsync "$@"
        }
        remote_rsync=${shell.escape (concatStringsSep " && " [
          "mkdir -m 0700 -p ${shell.escape plan.dst.path}/current"
          "exec flock -n ${shell.escape plan.dst.path} rsync"
        ])}
      '';
      pull = ''
        identity=${shell.escape plan.dst.host.ssh.privkey.path}
        src_user=root
        src_host=$(${fastest-address plan.src.host})
        src_port=$(${network-ssh-port plan.src.host "$src_host"})
        src_path=${shell.escape plan.src.path}
        src=$src_user@$src_host:$src_path
        dst_path=${shell.escape plan.dst.path}
        dst=$dst_path
        echo "update snapshot: current; $dst <- $src" >&2
        dst_shell() {
          eval "$with_dst_path_lock_script"
        }
        rsh="ssh -F /dev/null -i $identity ''${src_port:+-p $src_port}"
        local_rsync() {
          mkdir -m 0700 -p ${shell.escape plan.dst.path}/current
          flock -n ${shell.escape plan.dst.path} rsync "$@"
        }
        remote_rsync=rsync
      '';
    }}
    # Note that this only works because we trust date +%s to produce output
    # that doesn't need quoting when used to generate a command string.
    # TODO relax this requirement by selectively allowing to inject variables
    #   e.g.: ''${shell.quote "exec env NOW=''${shell.unquote "$NOW"} ..."}
    with_dst_path_lock_script="exec env start_date=$(date +%s) "${shell.escape
      "flock -n ${shell.escape plan.dst.path} /bin/sh"
    }
    local_rsync >&2 \
        -aAXF --delete \
        --rsh="$rsh" \
        --rsync-path="$remote_rsync" \
        --link-dest="$dst_path/current" \
        "$src/" \
        "$dst/.partial"
    dst_shell < ${toFile "backup.${plan.name}.take-snapshots" ''
      set -efu
      : $start_date

      dst=${shell.escape plan.dst.path}

      mv "$dst/current" "$dst/.previous"
      mv "$dst/.partial" "$dst/current"
      rm -fR "$dst/.previous"
      echo >&2

      snapshot() {(
        : $ns $format $retain
        name=$(date --date="@$start_date" +"$format")
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
    { ${pkgs.fping}/bin/fping </dev/null -a \
        ${concatMapStringsSep " " shell.escape
          (mapAttrsToList (_: net: head net.aliases) host.nets)} \
      | ${pkgs.coreutils}/bin/head -1; }
  '';

  # Note that we don't escape word on purpose, so we can deref shell vars.
  # TODO type word
  network-ssh-port = host: word: ''
    case ${word} in
    ${concatStringsSep ";;\n" (mapAttrsToList
      (_: net: "(${head net.aliases}) echo ${toString net.ssh.port}")
      host.nets)};;
    esac
  '';

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

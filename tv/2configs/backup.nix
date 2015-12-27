{ config, lib, pkgs, ... }:
with lib;
let
  # Users that are allowed to connect to the backup user.
  # Note: the user must own a push plan destination otherwise no rsync.
  backup-users = [
    config.krebs.users.tv
  ];

  ## TODO parse.file-location admit user
  ## loc has the form <host-name>:<abs-path>
  #parse.file-location = loc: let
  #  parts = splitString ":" loc;
  #  host-name = head parts;
  #  path = concatStringsSep ":" (tail parts);
  #in {
  #  type = "types.krebs.file-location";
  #  host = config.krebs.hosts.${host-name};
  #  path = path;
  #};

  # TODO assert plan.dst.path & co
  plans = with config.krebs.users; with config.krebs.hosts; addNames {
    xu-test-cd = {
      method = "push";
      #src = parse.file-location xu:/tmp/xu-test;
      #dst = parse.file-location cd:/krebs/backup/xu-test;
      src = { user = tv; host = xu; path = "/tmp/xu-test"; };
      dst = { user = tv; host = cd; path = "/krebs/backup/xu-test"; };
      startAt = "0,6,12,18:00";
      retain = {
        hourly = 4; # sneakily depends on startAt
        daily = 7;
        weekly = 4;
        monthly = 3;
      };
    };
    #xu-test-wu = {
    #  method = "push";
    #  dst = { user = tv; host = wu; path = "/krebs/backup/xu-test"; };
    #};
    cd-test-xu = {
      method = "pull";
      #src = parse.file-location cd:/tmp/cd-test;
      #dst = parse.file-location xu:/bku/cd-test;
      src = { user = tv; host = cd; path = "/tmp/cd-test"; };
      dst = { user = tv; host = xu; path = "/bku/cd-test"; };
    };

  };

  out = {
    #options.krebs.backup = api;
    config = imp;
  };

  imp = {
    users.groups.backup.gid = genid "backup";
    users.users = map makeUser (filter isPushDst (attrValues plans));
    systemd.services =
      flip mapAttrs' (filterAttrs (_:isPushSrc) plans) (name: plan: {
        name = "backup.${name}";
        value = makePushService plan;
      });
  };


  # TODO getFQDN: admit hosts in other domains
  getFQDN = host: "${host.name}.${config.krebs.search-domain}";

  isPushSrc = plan:
    plan.method == "push" &&
    plan.src.host.name == config.krebs.build.host.name;

  makePushService = plan: assert isPushSrc plan; {
    startAt = plan.startAt;
    serviceConfig.ExecStart = writeSh plan "rsync" ''
      exec ${pkgs.rsync}/bin/rsync ${concatMapStringsSep " " shell.escape [
        "-a"
        "-e"
        "${pkgs.openssh}/bin/ssh -F /dev/null -i ${plan.src.host.ssh.privkey.path}"
        "${plan.src.path}"
        "${plan.name}@${getFQDN plan.dst.host}::push"
      ]}
    '';
  };

  isPushDst = plan:
    plan.method == "push" &&
    plan.dst.host.name == config.krebs.build.host.name;

  makeUser = plan: assert isPushDst plan; rec {
    name = plan.name;
    uid = genid name;
    group = config.users.groups.backup.name;
    home = plan.dst.path;
    createHome = true;
    shell = "${writeSh plan "shell" ''
      case $2 in
        'rsync --server --daemon .')
          exec ${backup.rsync plan [ "--server" "--daemon" "." ]}
          ;;
        ''')
          echo "ERROR: no command specified" >&2
          exit 23
          ;;
        *)
          echo "ERROR: no unknown command: $SSH_ORIGINAL_COMMAND" >&2
          exit 23
          ;;
      esac
    ''}";
    openssh.authorizedKeys.keys = [ plan.src.host.ssh.pubkey ];
  };

  rsync = plan: args: writeSh plan "rsync" ''
    install -v -m 0700 -d ${plan.dst.path}/push >&2
    install -v -m 0700 -d ${plan.dst.path}/list >&2

    ${pkgs.rsync}/bin/rsync \
        --config=${backup.rsyncd-conf plan {
          post-xfer = writeSh plan "rsyncd.post-xfer" ''
            case $RSYNC_EXIT_STATUS in 0)
              exec ${backup.rsnapshot plan {
                preexec = writeSh plan "rsnapshot.preexec" ''
                  touch ${plan.dst.path}/rsnapshot.$RSNAPSHOT_INTERVAL
                '';
                postexec = writeSh plan "rsnapshot.postexec" ''
                  rm ${plan.dst.path}/rsnapshot.$RSNAPSHOT_INTERVAL
                '';
              }}
            esac
          '';
        }} \
        ${toString (map shell.escape args)}

    fail=0
    for i in monthly weekly daily hourly; do
      if test -e ${plan.dst.path}/rsnapshot.$i; then
        rm ${plan.dst.path}/rsnapshot.$i
        echo "ERROR: $i snapshot failed" >&2
        fail=1
      fi
    done
    if test $fail != 0; then
      exit -1
    fi
  '';

  rsyncd-conf = plan: conf: pkgs.writeText "${plan.name}.rsyncd.conf" ''
    fake super = yes
    use chroot = no
    lock file = ${plan.dst.path}/rsyncd.lock

    [push]
    max connections = 1
    path = ${plan.dst.path}/push
    write only = yes
    read only = no
    post-xfer exec = ${conf.post-xfer}

    [list]
    path = ${plan.dst.path}/list
    read only = yes
    write only = no
  '';

  rsnapshot = plan: conf: writeSh plan "rsnapshot" ''
    rsnapshot() {
      ${pkgs.proot}/bin/proot \
          -b /bin \
          -b /nix \
          -b /run/current-system \
          -b ${plan.dst.path} \
          -r ${plan.dst.path} \
          -w / \
          ${pkgs.rsnapshot}/bin/rsnapshot \
              -c ${pkgs.writeText "${plan.name}.rsnapshot.conf" ''
                config_version	1.2
                snapshot_root	${plan.dst.path}/list
                cmd_cp	${pkgs.coreutils}/bin/cp
                cmd_du	${pkgs.coreutils}/bin/du
                #cmd_rm	${pkgs.coreutils}/bin/rm
                cmd_rsync	${pkgs.rsync}/bin/rsync
                cmd_rsnapshot_diff	${pkgs.rsnapshot}/bin/rsnapshot-diff
                cmd_preexec	${conf.preexec}
                cmd_postexec	${conf.postexec}
                retain	hourly	4
                retain	daily	7
                retain	weekly	4
                retain	monthly	3
                lockfile	${plan.dst.path}/rsnapshot.pid
                link_dest	1
                backup	/push	./
                verbose	4
              ''} \
          "$@"
    }

    cd ${plan.dst.path}/list/

    now=$(date +%s)
    is_older_than() {
      test $(expr $now - $(date +%s -r $1 2>/dev/null || echo 0)) \
       -ge $2
    }

    # TODO report stale snapshots
    # i.e. there are $interval.$i > $interval.$max

    hour_s=3600
    day_s=86400
    week_s=604800
    month_s=2419200 # 4 weeks

    set --

    if test -e weekly.3 && is_older_than monthly.0 $month_s; then
      set -- "$@" monthly
    fi

    if test -e daily.6 && is_older_than weekly.0 $week_s; then
      set -- "$@" weekly
    fi

    if test -e hourly.3 && is_older_than daily.0 $day_s; then
      set -- "$@" daily
    fi

    if is_older_than hourly.0 $hour_s; then
      set -- "$@" hourly
    fi


    if test $# = 0; then
      echo "taking no snapshots" >&2
    else
      echo "taking snapshots: $@" >&2
    fi

    export RSNAPSHOT_INTERVAL
    for RSNAPSHOT_INTERVAL; do
      rsnapshot "$RSNAPSHOT_INTERVAL"
    done
  '';

  writeSh = plan: name: text: pkgs.writeScript "${plan.name}.${name}" ''
    #! ${pkgs.dash}/bin/dash
    set -efu
    export PATH=${makeSearchPath "bin" (with pkgs; [ coreutils ])}
    ${text}
  '';

in out

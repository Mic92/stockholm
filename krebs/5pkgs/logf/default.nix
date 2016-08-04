{ lib, pkgs, ... }:

with import <stockholm/lib>;

let
  default-host-colors = pkgs.writeJSON "logf.default-host-colors.json" {
  };
  default-prio-colors = pkgs.writeJSON "logf.default-prio-colors.json" {
    "0" = 196; # emerg
    "1" = 160; # alert
    "2" = 124; # crit
    "3" = 009; # err
    "4" = 011; # warning
    "5" = 255; # notice
    "6" = 250; # info
    "7" = 139; # debug
    "-" = 005; # undefined priority
  };
in

pkgs.writeDashBin "logf" ''
  export LOGF_HOST_COLORS LOGF_PRIO_COLORS
  LOGF_HOST_COLORS=$(cat "''${LOGF_HOST_COLORS-${default-host-colors}}")
  LOGF_PRIO_COLORS=$(cat "''${LOGF_PRIO_COLORS-${default-prio-colors}}")
  printf '%s\0' "$@" \
    | ${pkgs.findutils}/bin/xargs -0 -P 0 -n 1 ${pkgs.writeDash "logf-remote" ''
        target=$1
        target_host=$(echo "$1" | sed 's/^.*@//;s/\..*//')
        exec 3>&1
        2>&1 1>&3 ssh "$target" -T \
            -o PreferredAuthentications=publickey \
            -o StrictHostKeyChecking=yes \
            exec journalctl -af -n 0 -o json \
          | stdbuf -oL jq -Rcf ${pkgs.writeJq "logf-remote-error.jq" ''
              {
                PRIORITY: "4",
                MESSAGE: .,
                SYSLOG_IDENTIFIER: env.target_host,
              }
            ''}
        sleep 10m
        exec "$0" "$@"
      ''} \
    | ${pkgs.jq}/bin/jq -Rrf ${pkgs.writeJq "logf-filter.jq" ''
        (env.LOGF_HOST_COLORS | fromjson) as $host_colors |
        (env.LOGF_PRIO_COLORS | fromjson) as $prio_colors |

        def when(c; f): if c then f else . end;

        # anaphoric gsub
        def agsub(re; f):
          gsub("(?<it>\(re))"; .it | f);

        # :: [int] -> sgr
        def sgr: "\u001b[\(map(tostring) | join(";"))m";

        # :: sgr
        def rst: [] | sgr;

        # :: int -> sgr
        def fg(i): [38,5,i]|sgr;
        # TODO def fg(r;g;b): [38,2,r,g,b]|sgr;
        # http://cvs.schmorp.de/rxvt-unicode/src/command.C?revision=1.570&view=markup&sortby=log&sortdir=down

        # (sgr; sgr) | (null; any) :: str -> str
        def col(a; b): when(a != null; a + . + b);
        def col(a): col(a; rst);


        def p_time:
          ._SOURCE_REALTIME_TIMESTAMP
          | if . != null then . | fromjson | . / 1000000 else now end
          | gmtime
          | todateiso8601
          | col(fg(237));

        def p_host:
          ._HOSTNAME
          | if . != null then . else "-" end
          | col($host_colors[.]|when(. != null; fg(.)));

        def p_ident:
          if .SYSLOG_IDENTIFIER != null then .SYSLOG_IDENTIFIER
          else ._COMM end
          | col(fg(244));

        def p_message:
          fg($prio_colors[if has("PRIORITY") then .PRIORITY else "-" end])
            as $prio_c |
          .MESSAGE
          | sub("\r$"; "")
          | agsub("\\btv@nomic\\b"; "\(.)\u0007" | col(fg(219); $prio_c))
          #| agsub("Start queue"; "\(.)\u0007" | col(fg(42); $prio_c))
          | col($prio_c);

        try fromjson catch {
          _SOURCE_REALTIME_TIMESTAMP: now | tostring | sub("[.]"; ""),
          SYSLOG_IDENTIFIER: "logf/journalctl",
          MESSAGE: .,
        } |

        [ p_time
        , p_host
        , p_ident
        , p_message
        ]
        | join(" ")
      ''}
''

{ jq, stockholm, systemd, writeDashBin }:

let
  lib = stockholm.lib;
  user = "exim"; # TODO make this configurable
in

# TODO execute eximlog only if journalctl doesn't fail
# bash's set -o pipefail isn't enough

writeDashBin "eximlog" ''
  ${systemd}/bin/journalctl \
      -u ${lib.shell.escape user} \
      -o short-unix \
      "$@" \
    |
  ${jq}/bin/jq -Rr '
    # Only select lines that start with a timestamp
    select(test("^[0-9]")) |

    split(" ") |
    (.[0] | tonumber) as $time |
    (.[3:] | join(" ")) as $message |

    "\($time | strftime("%Y-%m-%d %H:%M:%S %z")) \($message)"

  '
''

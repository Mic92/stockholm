{ pkgs }:

pkgs.writeGawk "nixos-format-error" ''
  # usage: nixos-rebuild ... 2>&1 | nixos-format-error

  function out() {
    print
    next
  }

  BEGIN {
    IDLE = 0
    ACTIVE = 1
    PASSIVE = 2
    ERROR = 3

    start_state = IDLE

    state = start_state
  }

  END {
    if (trace_count)
      for (i = trace_count - 1; i >= 0; i--)
        print trace[i]
  }

  state == PASSIVE {
    out()
  }

  state == IDLE {
    if ($0 == "building the system configuration...") {
      state = ACTIVE
    }
    out()
  }

  state == ACTIVE {
    if ($1 == "error:") {
      state = ERROR
      sub(/^/,"\x1b[31;1m"); sub(/$/,"\x1b[m")
      trace[trace_count++] = $0

      "stty -F /dev/tty size" |& getline
      COLUMNS = gensub(/.* ([0-9]+)$/, "\\1", "1")

      next
    }
    if ($0 ~ /^these [0-9]+ derivations will be built:/) {
      state = PASSIVE
    }
    if ($0 == "activating the configuration...") {
      state = PASSIVE
    }
    out()
  }

  state == ERROR {

    if ($0 ~ /^\s*at /) {
      location = gensub(/^\s*at (.*):$/,"\\1","1")
      content = ""
      lnumcol = gensub(/^.*:([0-9]+:[0-9]+)$/,"\\1","1",location)
      lnum = gensub(/:.*/,"","1",lnumcol)
      col = gensub(/.*:/,"","1",lnumcol)
      next
    }

    if ($1 == lnum "|") {
      content = gensub(/^\s*[0-9]+\|(.*)/,"\\1","1")

      location = sprintf("%50s", location)

      preview_size = COLUMNS - length(location " ")

      prefix = gensub(/^\s*/,"","1",substr(content, 1, col))
      infix = gensub(/^([0-9a-zA-Z]+|.).*$/, "\\1", "1", substr(content, col + 1))
      suffix = substr(content, col + length(infix) + 1)

      if (length(prefix infix suffix) > preview_size) {
        n = (preview_size - length(infix)) / 2 - length(" ")
        prefix = substr(prefix, length(prefix) - n + 1)
        if (prefix != "") { prefix = "…" prefix }
        suffix = substr(suffix, 1, n)
        if (suffix != "") { suffix = suffix "…" }
      }

      preview = \
        "\x1b[38;5;244m" prefix "\x1b[m" \
        "\x1b[38;5;230m" infix "\x1b[m" \
        "\x1b[38;5;244m" suffix "\x1b[m"

      trace[trace_count++] = location " " preview
      next
    }

    if ($0 == "") next
    if ($0 ~ /^\s*… (from|while)/) next
    if ($0 ~ /^\s*([0-9]*)\|/) next

    trace[trace_count++] = $0
    next
  }
''

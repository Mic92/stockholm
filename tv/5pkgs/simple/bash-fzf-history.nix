with import <stockholm/lib>;
{ pkgs

, edit-key ? "ctrl-e"
, exec-key ? "enter"
, edit-mark ? "${mark-prefix}${edit-key}"
, exec-mark ? "${mark-prefix}${exec-key}"
, edit-command ? "\"\""
, exec-command ? "accept-line"
, mark-prefix ? " #FZFKEY:"
, finish-keyseq ? "\\C-x\\C-p"
, rebind-keyseq ? "\\C-x\\C-o"

, start-keyseq ? "\\C-f"
, load-keyseq ? start-keyseq
}: let
  script = pkgs.writeBash "bash-fzf-history.sh" ''
    if ! command -v fzf >/dev/null; then
      # Alternatively rewrite ${pkgs.fzf}/share/fzf/* to use absolute paths.
      fzf() {
        ${pkgs.fzf}/bin/fzf "$@"
      }
    fi

    . ${pkgs.fzf}/share/fzf/key-bindings.bash
    . ${pkgs.fzf}/share/fzf/completion.bash

    FZF_DEFAULT_OPTS='${toString [
      /* sh */ "--height=40%"
      /* sh */ "--inline-info"
      /* sh */ "--min-height=4"
      /* sh */ "--reverse"
    ]}'

    __fzf_history__() (
      result=$(
        HISTTIMEFORMAT= history |
        FZF_DEFAULT_OPTS="${toString [
          /* sh */ "--tac"
          /* sh */ "--sync"
          /* sh */ "-n2..,.."
          /* sh */ "--tiebreak=index"
          /* sh */ "--bind=ctrl-r:toggle-sort"
          /* sh */ "--expect=${edit-key},${exec-key}"
          /* sh */ "$FZF_DEFAULT_OPTS"
          /* sh */ "+m"
        ]}" \
        ${pkgs.fzf}/bin/fzf
      )
      if test -n "$result"; then
        shopt -s extglob

        key=''${result%%$'\n'*}
        line=''${result##*([^0-9])}
        index=''${line%%[^0-9]*}
        command=''${line##*([0-9 ])}

        echo "$command${mark-prefix}$key"
      else
        # Ensure no empty new line gets produced when fzf was aborted.
        echo '${edit-mark}'
      fi
    )

    __fzf_rebind_finish_keyseq__() {
      local suffix=
      case $READLINE_LINE in
        *'${edit-mark}')
          suffix='${edit-mark}'
          bind '"${finish-keyseq}": ${edit-command}'
          ;;
        *'${exec-mark}')
          suffix='${exec-mark}'
          bind '"${finish-keyseq}": ${exec-command}'
          ;;
      esac
      READLINE_LINE=${"\${READLINE_LINE:0:-\${#suffix}}"}
    }
    bind -x '"${rebind-keyseq}": __fzf_rebind_finish_keyseq__'

    bind '"\C-r": reverse-search-history'
    bind '"${start-keyseq}": " \C-e\C-u\C-y\ey\C-u`__fzf_history__`\e\C-e\er\e^${rebind-keyseq}${finish-keyseq}"'

    echo '# fzf key bindings loaded:' >&2
    bind -s | ${pkgs.gnugrep}/bin/grep __fzf_ >&2
  '';
in
  script //
  rec {
    bind = /* sh */ ''bind -x '"${load-keyseq}": . ${script}' '';
  }

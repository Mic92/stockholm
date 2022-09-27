{ coreutils, quote, stockholm, util-linux, writeDash }:
with stockholm.lib;

opt-spec: cmd-spec: let

  cmd = cmd-spec opts;

  cmd-script =
    if typeOf cmd == "set"
      then "exec ${cmd}"
      else cmd;

  opts = mapAttrs (name: value: value // rec {
    long = value.long or (replaceStrings ["_"] ["-"] name);
    ref = value.ref or "\"\$${varname}\"";
    short = value.short or null;
    switch = value.switch or false;
    varname = value.varname or (replaceStrings ["-"] ["_"] name);
  }) opt-spec;

  # true if b requires a to define its default value
  opts-before = a: b:
    test ".*[$]${stringAsChars (c: "[${c}]") a.varname}([^0-9A-Za-z_].*)?" (b.default or "");

  opts-list = let
    sort-out = toposort opts-before (attrValues opts);
  in
    if sort-out ? result
      then sort-out.result
      else throw "toposort output: ${toJSON sort-out}";

  wrapper-name =
    if typeOf cmd == "set" && cmd ? name
      then "${cmd.name}-getopt"
      else "getopt-wrapper";

in writeDash wrapper-name ''
  set -efu

  wrapper_name=${shell.escape wrapper-name}

  ${concatStringsSep "\n" (mapAttrsToList (name: opt: /* sh */ ''
    unset ${opt.varname}
  '') opts)}

  WITHGETOPT_ORIG_ARGS=$(${quote}/bin/quote "$@")
  export WITHGETOPT_ORIG_ARGS

  args=$(${util-linux}/bin/getopt \
      -l ${shell.escape
            (concatMapStringsSep ","
              (opt: opt.long + optionalString (!opt.switch) ":")
              (filter (opt: opt.long != null)
                      (attrValues opts)))} \
      -n "$wrapper_name" \
      -o ${shell.escape
            (concatMapStringsSep ""
              (opt: opt.short + optionalString (!opt.switch) ":")
              (filter (opt: opt.short != null)
                      (attrValues opts)))} \
      -s sh \
      -- "$@")
  if \test $? != 0; then exit 1; fi
  eval set -- "$args"

  while :; do
    case $1 in
    ${concatStringsSep "\n" (mapAttrsToList (name: opt: /* sh */ ''
      (${concatMapStringsSep "|" shell.escape (filter (x: x != "") [
        (optionalString (opt.long != null) "--${opt.long}")
        (optionalString (opt.short != null) "-${opt.short}")
      ])})
        ${if opt.switch then /* sh */ ''
          ${opt.varname}=true
          shift
        '' else /* sh */ ''
          ${opt.varname}=$2
          shift 2
        ''}
      ;;
    '') (filterAttrs
          (_: opt: opt.long != null || opt.short != null)
          opts))}
    --)
      shift
      break
    esac
  done

  ${concatMapStringsSep "\n"
    (opt: /* sh */ ''
      if \test "''${${opt.varname}+1}" != 1; then
        printf '%s: missing mandatory option '--%s'\n' \
            "$wrapper_name" \
            ${shell.escape opt.long}
        error=1
      fi
    '')
    (filter
      (x: ! hasAttr "default" x)
      (attrValues opts))}
  if test "''${error+1}" = 1; then
    exit 1
  fi

  ${concatMapStringsSep "\n"
    (opt: /* sh */ ''
      if \test "''${${opt.varname}+1}" != 1; then
        ${opt.varname}=${opt.default}
      fi
    '')
    (filter
      (hasAttr "default")
      opts-list)}

  ${concatStringsSep "\n" (mapAttrsToList (name: opt: /* sh */ ''
    export ${opt.varname}
  '') opts)}

  ${cmd-script} "$@"
''

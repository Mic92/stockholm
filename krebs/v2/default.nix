{ source
, target-user ? "root"
, target-host
, target-path ? "/var/src"
}:
with import <nixpkgs/lib>;
with import ~/stockholm/krebs/4lib {
  lib = import <nixpkgs/lib>;
};
with builtins;
let
  out = {
    inherit populate;
  };

  populate = ''
    #! /bin/sh
    set -eu

    verbose() {
      printf '+' >&2
      printf ' %q' "$@" >&2
      printf '\n'
      "$@"
    }

    echo ${shell.escape git-script} \
      | ssh ${shell.escape "${target-user}@${target-host}"} -T

    unset tmpdir
    trap '
      rm "$tmpdir"/*
      rmdir "$tmpdir"
      trap - EXIT INT QUIT
    '        EXIT INT QUIT
    tmpdir=$(mktemp -dt stockholm.XXXXXXXX)
    chmod 0755 "$tmpdir"

    ${concatStringsSep "\n"
      (mapAttrsToList
        (name: spec: let dst = removePrefix "symlink:" (get-url spec); in
          "verbose ln -s ${shell.escape dst} $tmpdir/${shell.escape name}")
        symlink-specs)}

    verbose proot \
        -b $tmpdir:${shell.escape target-path} \
        ${concatStringsSep " \\\n    "
          (mapAttrsToList
            (name: spec:
              "-b ${shell.escape "${get-url spec}:${target-path}/${name}"}")
            file-specs)} \
        rsync \
            -f ${shell.escape "P /*"} \
            ${concatMapStringsSep " \\\n        "
              (name: "-f ${shell.escape "R /${name}"}")
              (attrNames file-specs)} \
            --delete \
            -vFrlptD \
            ${shell.escape target-path}/ \
            ${shell.escape "${target-user}@${target-host}:${target-path}"}
  '';

  get-schema = uri:
    if substring 0 1 uri == "/"
      then "file"
      else head (splitString ":" uri);

  has-schema = schema: uri: get-schema uri == schema;

  get-url = spec: {
    string = spec;
    path = toString spec;
    set = get-url spec.url;
  }.${typeOf spec};

  git-specs =
    filterAttrs (_: spec: has-schema "https" (get-url spec)) source //
    filterAttrs (_: spec: has-schema "http" (get-url spec)) source //
    filterAttrs (_: spec: has-schema "git" (get-url spec)) source;

  file-specs =
    filterAttrs (_: spec: has-schema "file" (get-url spec)) source;

  symlink-specs =
    filterAttrs (_: spec: has-schema "symlink" (get-url spec)) source;

  git-script = ''
    #! /bin/sh
    set -efu

    verbose() {
      printf '+' >&2
      printf ' %q' "$@" >&2
      printf '\n'
    }

    fetch_git() {(
      dst_dir=$1
      src_url=$2
      src_ref=$3

      if ! test -e "$dst_dir"; then
        git clone "$src_url" "$dst_dir"
      fi

      cd "$dst_dir"

      if ! url=$(git config remote.origin.url); then
        git remote add origin "$src_url"
      elif test "$url" != "$src_url"; then
        git remote set-url origin "$src_url"
      fi

      # TODO resolve src_ref to commit hash
      hash=$src_ref

      if ! test "$(git log --format=%H -1)" = "$hash"; then
        git fetch origin
        git checkout "$hash" -- "$dst_dir"
        git checkout "$hash"
      fi

      git clean -dxf
    )}

    ${concatStringsSep "\n"
      (mapAttrsToList
        (name: spec: toString (map shell.escape [
          "verbose"
          "fetch_git"
          "${target-path}/${name}"
          spec.url
          spec.rev
        ]))
        git-specs)}
  '';

in out

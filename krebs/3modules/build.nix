{ config, lib, ... }:

with config.krebs.lib;

let
  out = {
    # TODO deprecate krebs.build.host
    options.krebs.build.host = mkOption {
      type = types.host;
    };

    # TODO make krebs.build.profile shell safe
    options.krebs.build.profile = mkOption {
      type = types.str;
      default = "/nix/var/nix/profiles/system";
    };

    # TODO deprecate krebs.build.user
    options.krebs.build.user = mkOption {
      type = types.user;
    };

    options.krebs.build.source = let
      raw = types.either types.str types.path;
      url = types.submodule {
        options = {
          url = mkOption {
            type = types.str;
          };
          rev = mkOption {
            type = types.str;
          };
          dev = mkOption {
            type = types.str;
          };
        };
      };
    in mkOption {
      type = types.attrsOf (types.either types.str url);
      apply = let f = mapAttrs (_: value: {
        string = value;
        path = toString value;
        set = f value;
      }.${typeOf value}); in f;
      default = {};
    };

    options.krebs.build.populate = mkOption {
      type = types.str;
      default = let
        source = config.krebs.build.source;
        target-user = maybeEnv "target_user" "root";
        target-host = maybeEnv "target_host" config.krebs.build.host.name;
        target-port = maybeEnv "target_port" "22";
        target-path = maybeEnv "target_path" "/var/src";
        out = ''
          #! /bin/sh
          set -eu

          verbose() {
            printf '+%s\n' "$(printf ' %q' "$@")" >&2
            "$@"
          }

          echo ${shell.escape git-script} \
            | ssh -p ${shell.escape target-port} \
                  ${shell.escape "${target-user}@${target-host}"} -T

          unset tmpdir
          trap '
            rm -f "$tmpdir"/*
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
                  -e ${shell.escape "ssh -p ${target-port}"} \
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
            printf '+%s\n' "$(printf ' %q' "$@")" >&2
            "$@"
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
      in out;
    };

  };

in out

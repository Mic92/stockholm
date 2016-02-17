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

    options.krebs.build.source = mkOption {
      type = with types; attrsOf (either str (submodule {
        options = {
          url = str;
          rev = str;
        };
      }));
      default = {};
    };

    options.krebs.build.populate = mkOption {
      type = types.str;
      default = let
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

          ${concatStringsSep "\n" (mapAttrsToList (name: symlink: ''
            verbose ln -s ${shell.escape symlink.target} \
                          "$tmpdir"/${shell.escape name}
          '') source-by-method.symlink)}

          verbose proot \
              -b "$tmpdir":${shell.escape target-path} \
              ${concatStringsSep " \\\n    " (mapAttrsToList (name: file:
                "-b ${shell.escape "${file.path}:${target-path}/${name}"}"
              ) source-by-method.file)} \
              rsync \
                  -f ${shell.escape "P /*"} \
                  ${concatMapStringsSep " \\\n        " (name:
                    "-f ${shell.escape "R /${name}"}"
                  ) (attrNames source-by-method.file)} \
                  --delete \
                  -vFrlptD \
                  -e ${shell.escape "ssh -p ${target-port}"} \
                  ${shell.escape target-path}/ \
                  ${shell.escape "${target-user}@${target-host}:${target-path}"}
        '';

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

          ${concatStringsSep "\n" (mapAttrsToList (name: git: ''
            verbose fetch_git ${concatMapStringsSep " " shell.escape [
              "${target-path}/${name}"
              git.url
              git.rev
            ]}
          '') source-by-method.git)}
        '';
      in out;
    };

  };

  source-by-method = let
    known-methods = ["git" "file" "symlink"];
  in genAttrs known-methods (const {}) // recursiveUpdate source-by-scheme {
    git = source-by-scheme.http or {} //
          source-by-scheme.https or {};
  };

  source-by-scheme = foldl' (out: { k, v }: recursiveUpdate out {
    ${v.scheme}.${k} = v;
  }) {} (mapAttrsToList (k: v: { inherit k v; }) normalized-source);

  normalized-source = mapAttrs (name: let f = x: getAttr (typeOf x) {
    path = f (toString x);
    string = f {
      url = if substring 0 1 x == "/" then "file://${x}" else x;
    };
    set = let scheme = head (splitString ":" x.url); in recursiveUpdate x {
      inherit scheme;
    } // {
      symlink.target = removePrefix "symlink:" x.url;
      file.path = # TODO file://host/...
                  assert hasPrefix "file:///" x.url;
                  removePrefix "file://" x.url;
    }.${scheme} or {};
  }; in f) config.krebs.build.source;
in out

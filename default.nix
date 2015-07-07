{ system-name
, rsync-target ? null
, deploy-target ? null
}:

# TODO assert that only one of rsync-target or deploy-target is not null

with builtins;
assert (typeOf system-name == "string");
with import <nixpkgs/lib>;
let
  paths-file = toPath "${dirOf __curPos.file}/modules/${system-name}/paths.nix";

  paths = import paths-file;

  prefetch.file = ''
    echo "$prefetch_in_url"
  '';

  prefetch.git = ''
    ${concatMapStringsSep "\n" (attr-name: ''
      case ''${prefetch_in_${escapeShellArg attr-name}-?} in \?)
        printf '%s: %s: missing attribute: %s' \
          ${escapeShellArg paths-file} \
          "$prefetch_name" \
          ${escapeShellArg attr-name} \
          >&2
        return 1
      esac
    '') [ "rev" "url" "cache" ]}

    git_rev=$prefetch_in_rev
    git_url=$prefetch_in_url

    # cache_dir points to a (maybe non-existent) directory, where a shared cache of
    # the repository should be maintained.  The shared cache is used to create
    # multiple working trees of the repository.
    cache_dir=$prefetch_in_cache/$(echo "$git_url" | urlencode)
    cache_git() {
      git --git-dir="$cache_dir" "$@"
    }

    # work_dir points to a (maybe non-existent) directory, where a specific
    # revision of the repository is checked out.
    # XXX this is probably a bad idea if git_rev is not a commit
    work_dir=$cache_dir-$(cache_git rev-parse --verify "$git_rev" | urlencode)
    work_git() {
      git -C "$work_dir" "$@"
    }

    is_up_to_date() {
      test -d "$cache_dir" &&
      test -d "$work_dir" &&
      test "$(cache_git rev-parse --verify "$git_rev")" = "$git_rev" &&
      test "$(work_git rev-parse --verify HEAD)" = "$git_rev"
    }

    # Notice how the remote name "origin" has been chosen arbitrarily, but must be
    # kept in sync with the default value of nixpkgs.rev.
    if ! is_up_to_date; then
      if ! test -d "$cache_dir"; then
        mkdir -p "$cache_dir"
        cache_git init --bare
      fi
      if ! cache_git_url=$(cache_git config remote.origin.url); then
        cache_git remote add origin "$git_url"
      elif test "$cache_git_url" != "$git_url"; then
        cache_git remote set-url origin "$git_url"
      fi
      cache_git fetch origin
      if ! test -d "$work_dir"; then
        git clone -n --shared "$cache_dir" "$work_dir"
      fi
      commit_name=$(cache_git rev-parse --verify "$git_rev")
      work_git checkout "$commit_name" -- "$(readlink -f "$work_dir")"
      work_git checkout -q "$commit_name"
      work_git submodule init
      work_git submodule update
    fi
    work_git clean -dxf

    echo "$work_dir"
  '';


  f = pkg-name: pkg-spec:
    let
      types = attrNames pkg-spec;
      type = elemAt types 0;
    in
    assert (length types == 1); # there can be only one source type
    ''
      out=$(${concatStringsSep " \\\n" (mapAttrsToList (k: v:
          "prefetch_in_${escapeShellArg k}=${escapeShellArg (toString v)}") pkg-spec.${type})} \
          prefetch_name=${escapeShellArg pkg-name} \
          __prefetch_${escapeShellArg type})
      printf '%s=%s\n' \
        ${escapeShellArg pkg-name} \
        "$out"
    '';
in
''
#! /bin/sh
set -euf

PATH=${toString ./.}/bin:$PATH
export PATH

__prefetch_file() {
${prefetch.file}
}
__prefetch_git() {
${prefetch.git}
}

# TODO make sure x contains only sane chars
x=$(${concatStrings (mapAttrsToList f paths)})

${optionalString (rsync-target != null) ''
  proot $(echo "$x" | sed -n 's@^\([^=]\+\)=\(.*\)@-b \2:/shitment/\1@p') \
    rsync --delete --delete-excluded \
      --filter='- /*/.git' \
      --rsync-path='mkdir -p -m 0700 /shitment/ && rsync' \
      -vaz \
      --no-owner \
      --no-group \
      '/shitment/' \
      ${escapeShellArg rsync-target}
''}


${optionalString (deploy-target != null) ''
  system_path=$(proot $(echo "$x" | sed -n 's@^\([^=]\+\)=\(.*\)@-b \2:/shitment/\1@p') \
    env \
        NIX_PATH=/shitment \
        NIXOS_CONFIG=/shitment/modules/${escapeShellArg system-name} \
      nix-build -A system --no-out-link '<nixpkgs/nixos>')

  system_name=${escapeShellArg system-name}
  target=${escapeShellArg deploy-target}

  nix-copy-closure --gzip --to "$target" "$system_path"

  secrets_root=${toString ./.}/secrets \
  config_root=${toString ./.} \
    copy-secrets "$system_name" "$target"

  ssh ''${NIX_SSHOPTS-} "$target" "$system_path/bin/switch-to-configuration" switch
''}

''

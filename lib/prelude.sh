# clone_or_update : [user@]hostname x local_dir x git_url x git_rev -> ()
clone_or_update() {(
  target=$1
  nixpkgs_dir=$2
  git_url=$3
  git_rev=$4

  echo '
    set -euf

    if [ ! -d "$nixpkgs_dir" ]; then
      mkdir -p "$nixpkgs_dir"
    fi

    cd "$nixpkgs_dir"

    git init -q

    if ! current_url=$(git config remote.src.url); then
      git remote add src "$git_url"
    elif [ $current_url != $git_url ]; then
      git remote set-url src "$git_url"
    fi

    git fetch src

    git checkout "$git_rev"
  ' \
    | ssh "$target" env \
          nixpkgs_dir="$nixpkgs_dir" \
          git_rev="$git_rev" \
          git_url="$git_url" \
        /bin/sh
)}

# deploy : nixos-config x [user@]hostname -> ()
deploy() {(
  main=$1
  target=$2
  nixpkgs_dir=/var/nixpkgs # TODO make configurable

  git_url=$(nixpkgs_url $main)
  git_rev=$(nixpkgs_rev $main)

  if [ "$git_url" = '' ] || [ "$git_rev" = '' ]; then
    echo "specify nixpkgs.url and nixpkgs.rev in $main !"
    exit 23
  fi

  filter=$(rsync_filter "$main")

  echo "$filter" \
    | rsync -f '. -' -zvrlptD --delete-excluded ./ "$target":/etc/nixos/

  clone_or_update "$target" "$nixpkgs_dir" "$git_url" "$git_rev"
  ssh "$target" nixos-rebuild switch -I nixos-config=/etc/nixos/"$main" -I nixpkgs="$nixpkgs_dir"

)}

# rsync_filter : nixos-config -> rsync-filter
rsync_filter() {(
  main=$1

  hosts=$(list_hosts)
  module_imports=$(set -euf; list_module_imports "$main")
  other_imports=$(
    echo "$module_imports" \
      | xargs grep -H . \
      | import_statements \
      | slash_path_relpath \
      | undot_paths \
      | sort \
      | uniq \
      | sed '/\.nix$/!s:$:/default.nix:' \
      )
  secrets=$(echo "$module_imports" | xargs cat | quoted_strings | filter_secrets)

  # TODO collect all other paths from *_imports

  abs_deps=$(
    echo "$hosts"
    echo "$module_imports"
    echo "$other_imports"
    echo "$secrets"
  )

  rel_deps=$(echo "$abs_deps" | make_relative_to "$PWD")
  filter=$(echo "$rel_deps" | make_rsync_whitelist)

  echo "$filter"
)}

# list_module_imports : nix-file -> lines nix-file
list_module_imports() {
  if echo "$1" | grep -q ^/; then
    :
  else
    set -- "./$1"
  fi
  imports=$(nix-instantiate \
      --strict \
      --json \
      --eval \
      -E \
      "with builtins; with import ./lib/modules.nix; map toString (list-imports $1)")
  echo "$imports" \
    | jq -r .[]
}

# list_hosts : lines tinc-host-file
# Precondition: $PWD/hosts is the correct repository :)
list_hosts() {
  git -C hosts ls-tree --name-only HEAD \
    | awk '{print ENVIRON["PWD"]"/hosts/"$$0}'
}

# filter_secrets : lines string |> lines secrets-file-candidate
# Notice how false positives are possible.
filter_secrets() {
  sed -n 's:^\(.*/\)\?\(secrets/.*\):'"${PWD//:/\\:}"'/\2:p'
}

# import_statements : lines (path ":" string) |> lines (path ":" relpath)
import_statements() {
  sed -n '
        s@^\([^:]\+:\)\('"$(bre_invert_word import)"'\)*\<import\s\+@\1@
        t1;d
    :1; s@^\([^:]\+:\)\(\.*/\S*\)@\1\2\n@
        t2;d
    :2; P;D
  '
}

# slash_path_relpath : lines (path ":" relpath) |> lines path
#
# Example: "/foo/bar: baz" => "/foo/baz"
#
slash_path_relpath() {
  sed -n 's@/[^/]\+:@/@p'
}

# undot_paths : lines path |> lines path
# Remove all dots (. and ..) from input paths.
undot_paths() {
  sed '
    :0
    s://\+:/:g
    s:/\.\(/\|$\):\1:g
    s:/[^/]\+/\.\.\(/\|$\):\1:g
    s:^/\(\.\./\)\+:/:
    t0
    s:^$:/:
  '
}

# quoted_strings : lines string |> lines string
# Extract all (double-) quoted strings from stdin.
#
# 0. find begin of string or skip line
# 1. find end of string or skip line
# 2. print string and continue after string
quoted_strings() {
  sed '
        s:[^"]*"::                  ;t1;d
    :1; s:\(\([^"]\|\\"\)*\)":\1\n: ;t2;d
    :2; P;D
  ' \
    | sed 's:\\":":g'
}

# bre_escape : lines string |> lines bre-escaped-string
bre_escape() {
  sed 's:[\.\[\\\*\^\$]:\\&:g'
}

# bre_invert_word : string -> BRE
# TODO escape chars in the resulting BRE.
bre_invert_word() {
  awk -v input="$1" '
    BEGIN {
      split(input,s,"")
      for (i in s) {
        c=s[i]
        printf "\\|%s[^%s]", y, c
        y = y c
      }
    }
  '
}

# ls_bre : directory -> BRE
# Create a BRE from the files in a directory.
ls_bre() {
  ls "$1" \
    | tr \\n / \
    | sed '
        s:[\.\[\\\*\^\$]:\\&:g
        s:/$::
        s:/:\\|:g
      '
}

# make_relative_to : lines path |> directory -> lines path
# Non-matching paths won't get altered.
make_relative_to() {
  sed "s:^$(echo "$1/" | bre_escape | sed 's/:/\\:/g')::"
}

# make_rsync_whitelist : lines relpath |> liens rsync-filter
make_rsync_whitelist() {
  set -- "$(cat)"

  # include all files in stdin and their directories
  {
    echo "$1"
    echo "$1" | make_parent_dirs | sort | uniq
  } \
    | sed 's|^|+ /|'

  # exclude everything else
  echo '- *'
}

# make_parent_dirs : lines path |> lines directory
# List all parent directories of a path.
make_parent_dirs() {
  set -- "$(sed -n 's|/[^/]*$||p' | grep . | sort | uniq)"
  if echo "$1" | grep -q .; then
    echo "$1"
    echo "$1" | make_parent_dirs
  fi
}

# nixpkgs_url : nixos-config -> git_url
nixpkgs_url() {
  nix-instantiate \
      -I nixos-config="$1" \
      --eval \
      --json \
      -E '(import <nixos-config> {config={}; pkgs={};}).nixpkgs.url' 2> /dev/null \
    | jq -r .
}

# nixpkgs_rev : nixos-config -> git_rev
nixpkgs_rev() {
  nix-instantiate \
      -I nixos-config="$1" \
      --eval \
      --json \
      -E '(import <nixos-config> {config={}; pkgs={};}).nixpkgs.rev' 2> /dev/null \
    | jq -r . 2> /dev/null
}

# verbose COMMAND [ARGS...]
verbose() {
  echo "$@" >&2
  "$@"
}

{ coreutils, git, stdenv, writeDashBin }:

writeDashBin "git-preview" ''
  PATH=${stdenv.lib.makeBinPath [
    coreutils
    git
  ]}''${PATH+:$PATH}
  hashes=$(git log --format=%h "..$1")
  end=$(echo "$hashes" | head -1)
  start=$(echo "$hashes" | tail -1)
  # exit if no diff was found
  test -z "$start" && exit 0
  shift
  git diff "$start^..$end" "$@"
''

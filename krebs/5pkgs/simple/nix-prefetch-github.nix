{ curl, jq, nix, writeDashBin }:

writeDashBin "nix-prefetch-github" ''
  # usage: nix-prefetch-github OWNER REPO [REF]
  set -efu

  owner=$1
  repo=$2
  ref=''${3-master}

  info_url=https://api.github.com/repos/$owner/$repo/commits/$ref
  info=$(${curl}/bin/curl -fsS "$info_url")
  rev=$(printf %s "$info" | ${jq}/bin/jq -r .sha)

  name=$owner-$repo-$ref
  url=https://github.com/$owner/$repo/tarball/$rev
  sha256=$(${nix}/bin/nix-prefetch-url --name "$name" --unpack "$url")

  export owner repo rev sha256
  ${jq}/bin/jq -n '
    env | {
      owner, repo, rev, sha256
    }
  '
''

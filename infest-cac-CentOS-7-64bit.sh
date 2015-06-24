#! /bin/sh
set -xeuf

serverspec=$1
systemname=$2

(
  PATH=$PWD/bin:$PATH
  export PATH

  # Notice NIX_PATH used from host
  # Notice secrets required to evaluate configuration
  NIX_PATH=$NIX_PATH:nixos-config=$PWD/modules/$systemname
  NIX_PATH=$NIX_PATH:secrets=$PWD/secrets/$systemname/nix
  export NIX_PATH

  rev=$(nixos-query nixpkgs.rev)
  url=$(nixos-query nixpkgs.url)

  fetchgit "$rev" "$url" tmp/nixpkgs/$systemname
)

./cac poll 10s 2>/dev/null &
pollpid=$!
trap "kill $pollpid; trap - EXIT" EXIT

./cac waitstatus $serverspec 'Powered On'

# TODO don't set label/mode if they're already good
./cac setlabel $serverspec $systemname
./cac setmode $systemname normal
./cac generatenetworking $systemname > modules/$systemname/networking.nix

cat infest.d/cac-CentOS-7-64bit/prepare.sh | ./cac ssh $systemname \
  nix_url=https://nixos.org/releases/nix/nix-1.9/nix-1.9-x86_64-linux.tar.bz2 \
  nix_sha256=5c76611c631e79aef5faf3db2d253237998bbee0f61fa093f925fa32203ae32b \
  /bin/sh

./cac pushconfig $systemname /mnt

# This needs to be run twice because (at least):
#   Initialized empty Git repository in /var/lib/git/$reponame
#   chown: invalid user: 'git:nogroup'
cat infest.d/nixos-install.sh | ./cac ssh $systemname || :
cat infest.d/nixos-install.sh | ./cac ssh $systemname

cat infest.d/cac-CentOS-7-64bit/finalize.sh | ./cac ssh $systemname

./cac powerop $systemname reset

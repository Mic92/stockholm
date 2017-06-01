{ stdenv, fetchgit, cmake, ncurses, openssl, readline, ... }:

stdenv.mkDerivation rec {
  name = "tarantool-1.7.1-164-g0fd0239";
  src = fetchgit {
    url = https://github.com/tarantool/tarantool;
    rev = builtins.elemAt (builtins.match ".*-g([0-9a-f]+)" name) 0;
    sha256 = "1jnaiizbl9j4a8vsihqx75iqa9bkh1kpwsyrgmim8ikiyzfw54dz";
    fetchSubmodules = true;
  };
  buildInputs = [
    cmake
    ncurses
    openssl
    readline
  ];
  preConfigure = ''
    echo ${(builtins.parseDrvName name).version} > VERSION
    sed -i 's/NAMES termcap/NAMES ncurses/' cmake/FindTermcap.cmake
  '';
}

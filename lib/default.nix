let
  lib = import <nixpkgs/lib> // builtins // {
    shell = import ./shell.nix { inherit lib; };

    eq = x: y: x == y;
    ne = x: y: x != y;
    mod = x: y: x - y * (x / y);
  };
in

lib

let
  lib = import <nixpkgs/lib> // builtins // {
    shell = import ./shell.nix { inherit lib; };
  };
in

lib

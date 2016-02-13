{ system ? builtins.currentSystem }:

(import <stockholm> {
  inherit system;
  configuration = {};
}).pkgs

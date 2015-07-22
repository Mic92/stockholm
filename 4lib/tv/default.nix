{ lib, pkgs, ... }:

with builtins;
with lib;

builtins // lib // rec {

  git = import ./git.nix {
    lib = lib // {
      inherit addNames;
    };
    inherit pkgs;
  };

  addName = name: set:
    set // { inherit name; };

  addNames = mapAttrs addName;


  # "7.4.335" -> "74"
  majmin = with lib; x : concatStrings (take 2 (splitString "." x));


  concat = xs :
    if xs == []
      then ""
      else head xs + concat (tail xs)
    ;

  flip = f : x : y : f y x;

  # isSuffixOf :: String -> String -> Bool
  isSuffixOf =
    s : xs :
      let
        sn = stringLength s;
        xsn = stringLength xs;
      in
        xsn >= sn && substring (xsn - sn) sn xs == s ;

  # setMap :: (String -> a -> b) -> Set String a -> [b]
  #setMap = f: xs: map (k : f k (getAttr k xs)) (attrNames xs);

  # setToList :: Set k a -> [a]
  #setToList = setMap (_: v: v);

  shell-escape =
    let
      isSafeChar = c: match "[-./0-9_a-zA-Z]" c != null;
    in
    stringAsChars (c:
      if isSafeChar c then c
      else if c == "\n" then "'\n'"
      else "\\${c}");

  types = lib.types // (with lib.types; rec {

    host = submodule {
      options = {
        name = mkOption {
          type = label;
        };
        dc = mkOption {
          type = label;
        };
        cores = mkOption {
          type = positive;
        };
        nets = mkOption {
          type = attrsOf net;
          apply = x: assert hasAttr "retiolum" x; x;
        };
      };
    };

    net = submodule ({ config, ... }: {
      options = {
        addrs = mkOption {
          type = listOf addr;
          apply = _: config.addrs4 ++ config.addrs6;
        };
        addrs4 = mkOption {
          type = listOf addr4;
          default = [];
        };
        addrs6 = mkOption {
          type = listOf addr6;
          default = [];
        };
        aliases = mkOption {
          # TODO nonEmptyListOf hostname
          type = listOf hostname;
        };
        tinc-key = mkOption {
          type = str;
        };
      };
    });

    positive = mkOptionType {
      name = "positive integer";
      check = x: isInt x && x > 0;
      merge = mergeOneOption;
    };

    # TODO
    addr = str;
    addr4 = str;
    addr6 = str;
    hostname = str;
    label = str;
  });

}

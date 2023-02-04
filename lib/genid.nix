{ lib, ... }:
with lib;
with builtins;
let out = genid;

  # id = genid s = (hash s + min) % max
  # min <= genid s < max
  #
  # min = 2^24 =   16777216 = 0x001000000
  # max = 2^32 = 4294967296 = 0x100000000
  #
  # id is bigger than UID of nobody and GID of nogroup
  # see <nixos/modules/misc/ids.nix> and some spare for stuff like lxd.
  #
  # :: str -> uint32
  genid = s: sum16 (addmod16_16777216 (hash s));

  # :: str -> list8 uint4
  hash = s:
    map hexint (stringToCharacters (substring 32 8 (hashString "sha1" s)));

  # :: list uint -> uint
  sum16 = foldl (a: i: a * 16 + i) 0;

  # :: list8 uint4 -> list1 uint8 ++ list6 uint4
  addmod16_16777216 = x: let
    a = 16 * head x + head (tail x);
    d = tail (tail x);
  in [(mod (a + 1) 256)] ++ d;

  # :: char -> uint4
  hexint = x: hexvals.${toLower x};

  # :: attrset char uint4
  hexvals = listToAttrs (imap (i: c: { name = c; value = i - 1; }) hexchars);
in out

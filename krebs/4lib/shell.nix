{ lib, ... }:

with builtins;
with lib;

rec {
  escape =
    let
      isSafeChar = c: match "[-./0-9_a-zA-Z]" c != null;
    in
    stringAsChars (c:
      if isSafeChar c then c
      else if c == "\n" then "'\n'"
      else "\\${c}");

  #
  # shell script generators
  #

  # example: "${cat (toJSON { foo = "bar"; })} | jq -r .foo"
  cat = s: "printf '%s' ${escape s}";
}

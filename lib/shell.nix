{ lib, ... }:

with lib;

rec {
  escape =
    let
      isSafeChar = testString "[-+./0-9:=A-Z_a-z]";
    in
      x:
        if x == "" then "''"
        else stringAsChars (c:
          if isSafeChar c then c
          else if c == "\n" then "'\n'"
          else "\\${c}"
        ) x;

  #
  # shell script generators
  #

  # example: "${cat (toJSON { foo = "bar"; })} | jq -r .foo"
  cat = s: "printf '%s' ${escape s}";
}

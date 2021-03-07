{ pkgs }:

pkgs.mpv-unwrapped.overrideAttrs (old: rec {
  pname = "mpvterm";
  patches = old.patches or [] ++ [
    ./mpvterm.patch
  ];
})

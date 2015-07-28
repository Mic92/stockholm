{ lib, ... }:

with lib;

rec {
  # listset k v = set k [v]

  # insert : k -> v -> listset k v -> listset k v
  insert = name: value: set:
    set // { ${name} = set.${name} or [] ++ [value]; };
}

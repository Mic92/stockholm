{ writeText, ... }:
writeText "cac.pem" (builtins.readFile ./cac.pem)

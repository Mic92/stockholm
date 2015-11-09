{ lib, ... }:

let
  inherit (lib) addNames escapeShellArg makeSearchPath optionalString;

  commands = addNames {
    git-receive-pack = {};
    git-upload-pack = {};
  };

  receive-modes = addNames {
    fast-forward = {};
    non-fast-forward = {};
    create = {};
    delete = {};
    merge = {}; # TODO implement in git.nix
  };

  permissions = {
    fetch = {
      allow-commands = [
        commands.git-upload-pack
      ];
    };

    push = ref: extra-modes: {
      allow-commands = [
        commands.git-receive-pack
        commands.git-upload-pack
      ];
      allow-receive-ref = ref;
      allow-receive-modes = [ receive-modes.fast-forward ] ++ extra-modes;
    };
  };

  refs = {
    master = "refs/heads/master";
    all-heads = "refs/heads/*";
  };

in
commands // receive-modes // permissions // refs

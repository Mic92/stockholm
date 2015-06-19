{ ... }:

let
  inherit (builtins) readFile;
in

{
  users.extraUsers =
    {
      root = {
        openssh.authorizedKeys.keys = [
          (readFile <pubkeys/deploy_wu.ssh.pub>)
          (readFile <pubkeys/tv_wu.ssh.pub>)
        ];
      };
    };

  users.mutableUsers = false;
}

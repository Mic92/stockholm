{ pkgs }:

let
  pass = pkgs.pass.withExtensions (ext: [
    ext.pass-otp
  ]);
in

pkgs.write "brain" {
  "/bin/brain".link = pkgs.writeDash "brain" ''
    PASSWORD_STORE_DIR=$HOME/brain \
    exec ${pass}/bin/pass "$@"
  '';
  "/bin/brainmenu".link = pkgs.writeDash "brainmenu" ''
    PASSWORD_STORE_DIR=$HOME/brain \
    exec ${pass}/bin/passmenu "$@"
  '';
  "/share/bash-completion/completions/brain".link =
    pkgs.runCommand "brain-completions" {
    } /* sh */ ''
      sed -r '
        s/\<_pass?(_|\>)/_brain\1/g
        s/\<__password_store/_brain/g
        s/\<pass\>/brain/
        s/\$HOME\/\.password-store/$HOME\/brain/
      ' < ${pass}/share/bash-completion/completions/pass > $out
    '';
}

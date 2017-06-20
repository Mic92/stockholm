{ pass, writeOut, writeDash, ... }:

writeOut "brain" {
  "/bin/brain".link = writeDash "brain" ''
    PASSWORD_STORE_DIR=$HOME/brain \
    exec ${pass}/bin/pass $@
  '';
  "/bin/brainmenu".link = writeDash "brainmenu" ''
    PASSWORD_STORE_DIR=$HOME/brain \
    exec ${pass}/bin/passmenu $@
  '';
}

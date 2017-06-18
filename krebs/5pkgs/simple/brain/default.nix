{ pass, writeOut, writeDash, ... }:

writeOut "brain" {
  "/bin/brain-pass".link = writeDash "brain-pass" ''
    PASSWORD_STORE_DIR=$HOME/brain \
    exec ${pass}/bin/pass $@
  '';
  "/bin/brain-passmenu".link = writeDash "brain-passmenu" ''
    PASSWORD_STORE_DIR=$HOME/brain \
    exec ${pass}/bin/passmenu $@
  '';
}

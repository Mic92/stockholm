{ pass, write, writeDash, ... }:

write "dsco-pass" {
  "/bin/dpass".link = writeDash "dpass" ''
    PASSWORD_STORE_DIR=$HOME/.dpasswordstore \
    exec ${pass}/bin/pass $@
  '';
  "/bin/dpassmenu".link = writeDash "dpassmenu" ''
    PASSWORD_STORE_DIR=$HOME/.dpasswordstore \
    exec ${pass}/bin/passmenu $@
  '';
}

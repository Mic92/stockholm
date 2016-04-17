{ config, pkgs, ... }: with config.krebs.lib;

pkgs.writeScriptBin "get-ssh-port" ''
  #! ${pkgs.dash}/bin/dash
  set -efu
  if test $# != 1 || test $1 = -h || test $1 = --help; then
    echo "usage: get-ssh-port HOSTNAME" >&2
    exit 23
  fi
  case $1 in
  ${concatMapStringsSep ";;\n"
    (host: toString [
      "(${shell.escape host.name})"
      "echo ${toString host.nets.${config.krebs.search-domain}.ssh.port}"
    ])
    (filter (host: hasAttr config.krebs.search-domain host.nets)
            (attrValues config.krebs.hosts))
  };;
  ${concatMapStringsSep ";;\n"
    (net: toString [
      "(${concatMapStringsSep "|" shell.escape net.aliases})"
      "echo ${toString net.ssh.port}"
    ])
    (concatMap (host: attrValues host.nets) (attrValues config.krebs.hosts))
  };;
  (*) echo "get-ssh-port: don't know ssh port of $1" >&2
      exit 1
  esac
''

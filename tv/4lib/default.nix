{ lib, ... }:

{
  # TODO deprecate shell-escape for lass
  shell-escape = lib.shell.escape;
}

{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

{
  programs.bash = {
    interactiveShellInit = ''
      HISTCONTROL='erasedups:ignorespace'
      HISTSIZE=65536
      HISTFILESIZE=$HISTSIZE

      shopt -s checkhash
      shopt -s histappend histreedit histverify
      shopt -s no_empty_cmd_completion
      complete -d cd
    '';
    promptInit = ''
      case $UID in
        0)
          PS1='\[\e[1;31m\]\w\[\e[0m\] '
          ;;
        ${toString config.krebs.build.user.uid})
          PS1='\[\e[1;32m\]\w\[\e[0m\] '
          ;;
        *)
          PS1='\[\e[1;35m\]\u \[\e[1;32m\]\w\[\e[0m\] '
          ;;
      esac
      if test -n "$SSH_CLIENT"; then
        PS1='\[\e[35m\]\h'" $PS1"
      fi
      if test -n "$SSH_AGENT_PID"; then
        PS1="ssh-agent[$SSH_AGENT_PID] $PS1"
      fi

      case ''${XMONAD_SPAWN_WORKSPACE-} in
        stockholm)
          cd ~/stockholm
        ;;
      esac
    '';
  };
}

with import ./lib;
{ config, pkgs, ... }: {
  programs.bash = {
    interactiveShellInit = /* sh */ ''
      HISTCONTROL='erasedups:ignorespace'
      HISTSIZE=900001
      HISTFILESIZE=$HISTSIZE
      HISTTIMEFORMAT=

      shopt -s checkhash
      shopt -s histappend histreedit histverify
      shopt -s no_empty_cmd_completion
      complete -d cd

      case $UID in
        ${shell.escape (toString config.krebs.users.tv.uid)})
          if test ''${SHLVL-1} = 1; then
            case ''${XMONAD_SPAWN_WORKSPACE-} in
              stockholm)
                cd ~/stockholm
              ;;
            esac
          fi

          export NIX_PATH="stockholm=$HOME/stockholm:$NIX_PATH"
        ;;
      esac

      ${pkgs.bash-fzf-history.bind}
    '';
    promptInit = /* sh */ ''
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
    '';
  };
}

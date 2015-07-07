{ pkgs, ... }:

let
  lib = import ../../lib { lib = pkgs.lib; inherit pkgs; };

  inherit (lib) majmin;
in

{
  environment.systemPackages = with pkgs; [
    vim
  ];

  environment.etc."vim/vimrc".text = ''
    set nocp
  '';

  environment.etc."vim/vim${majmin pkgs.vim.version}".source =
      "${pkgs.vim}/share/vim/vim${majmin pkgs.vim.version}";

  # multiple-definition-problem when defining environment.variables.EDITOR
  environment.extraInit = ''
    EDITOR=vim
  '';

  environment.shellAliases = {
    # alias cal='cal -m3'
    gp = "${pkgs.pari}/bin/gp -q";
    df = "df -h";
    du = "du -h";
    # alias grep='grep --color=auto'

    # TODO alias cannot contain #\'
    # "ps?" = "ps ax | head -n 1;ps ax | fgrep -v ' grep --color=auto ' | grep";

    # alias la='ls -lA'
    lAtr = "ls -lAtr";
    # alias ll='ls -l'
    ls = "ls -h --color=auto --group-directories-first";
    # alias vim='vim -p'
    # alias vi='vim'
    # alias view='vim -R'
    dmesg = "dmesg -L --reltime";
  };

  environment.variables.VIM = "/etc/vim";

  programs.bash = {
    interactiveShellInit = ''
      HISTCONTROL='erasedups:ignorespace'
      HISTSIZE=65536
      HISTFILESIZE=$HISTSIZE

      shopt -s checkhash
      shopt -s histappend histreedit histverify
      shopt -s no_empty_cmd_completion
      complete -d cd

      # TODO source bridge
    '';
    promptInit = ''
      case $UID in
        0)
          PS1='\[\e[1;31m\]\w\[\e[0m\] '
          ;;
        1337)
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

  programs.ssh.startAgent = false;
}

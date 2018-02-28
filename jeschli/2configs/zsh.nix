{ config, lib, pkgs, ... }:
{
  environment.systemPackages = [ pkgs.fzf ];
  programs.zsh = {
    enable = true;
    shellInit = ''
      #disable config wizard
      zsh-newuser-install() { :; }
    '';
    interactiveShellInit = ''
      setopt autocd extendedglob
      bindkey -e

      #history magic
      bindkey "[A" up-line-or-local-history
      bindkey "[B" down-line-or-local-history

      up-line-or-local-history() {
          zle set-local-history 1
          zle up-line-or-history
          zle set-local-history 0
      }
      zle -N up-line-or-local-history
      down-line-or-local-history() {
          zle set-local-history 1
          zle down-line-or-history
          zle set-local-history 0
      }
      zle -N down-line-or-local-history

      setopt share_history
      setopt hist_ignore_dups
      # setopt inc_append_history
      bindkey '^R' history-incremental-search-backward

      #C-x C-e open line in editor
      autoload -z edit-command-line
      zle -N edit-command-line
      bindkey "^X^E" edit-command-line

      #fzf inclusion
      source ${pkgs.fzf}/share/fzf/completion.zsh
      source ${pkgs.fzf}/share/fzf/key-bindings.zsh

      #completion magic
      autoload -Uz compinit
      compinit
      zstyle ':completion:*' menu select

      #enable automatic rehashing of $PATH
      zstyle ':completion:*' rehash true

      eval $(dircolors -b ${pkgs.fetchFromGitHub {
        owner = "trapd00r";
        repo = "LS_COLORS";
        rev = "master";
        sha256="05lh5w3bgj9h8d8lrbbwbzw8788709cnzzkl8yh7m1dawkpf6nlp";
      }}/LS_COLORS)

      #beautiful colors
      alias ls='ls --color'
      # zstyle ':completion:*:default' list-colors ''${(s.:.)LS_COLORS}

      #emacs bindings
      bindkey "[7~" beginning-of-line
      bindkey "[8~" end-of-line
      bindkey "Oc" emacs-forward-word
      bindkey "Od" emacs-backward-word

      #aliases
      alias ll='ls -l'
      alias la='ls -la'

      #fancy window title magic
    '';
    promptInit = ''
      # TODO: figure out why we need to set this here
      HISTSIZE=900001
      HISTFILESIZE=$HISTSIZE
      SAVEHIST=$HISTSIZE

      autoload -U promptinit
      promptinit

      p_error='%(?..%F{red}%?%f )'
      t_error='%(?..%? )'

      case $UID in
        0)
          p_username='%F{red}root%f'
          t_username='root'
          ;;
        1337)
          p_username=""
          t_username=""
          ;;
        *)
          p_username='%F{blue}%n%f'
          t_username='%n'
          ;;
      esac

      if test -n "$SSH_CLIENT"; then
        p_hostname='@%F{magenta}%M%f '
        t_hostname='@%M '
      else
        p_hostname=""
        t_hostname=""
      fi

      #check if in nix shell
      if test -n "$buildInputs"; then
        p_nixshell='%F{green}[s]%f '
        t_nixshell='[s] '
      else
        p_nixshell=""
        t_nixshell=""
      fi

      PROMPT="$p_error$p_username$p_hostname$p_nixshell%~ "
      TITLE="$t_error$t_username$t_hostname$t_nixshell%~"
      case $TERM in
        (*xterm* | *rxvt*)
          function precmd {
            PROMPT_EVALED="$(print -P $TITLE)"
            echo -ne "\033]0;$$ $PROMPT_EVALED\007"
          }
          # This is seen while the shell waits for a command to complete.
          function preexec {
            PROMPT_EVALED="$(print -P $TITLE)"
            echo -ne "\033]0;$$ $PROMPT_EVALED $1\007"
          }
        ;;
      esac
    '';
  };
  users.defaultUserShell = "/run/current-system/sw/bin/zsh";
}

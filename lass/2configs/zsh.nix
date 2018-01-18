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
      case $TERM in
        (*xterm* | *rxvt*)
          function precmd {
            if test -n "$SSH_CLIENT"; then
              echo -ne "\033]0;$$ $USER@$HOST $PWD\007"
            else
              echo -ne "\033]0;$$ $USER@$PWD\007"
            fi
          }
          # This is seen while the shell waits for a command to complete.
          function preexec {
            if test -n "$SSH_CLIENT"; then
              echo -ne "\033]0;$$ $USER@$HOST $PWD $1\007"
            else
              echo -ne "\033]0;$$ $USER@$PWD $1\007"
            fi
          }
        ;;
      esac
    '';
    promptInit = ''
      # TODO: figure out why we need to set this here
      HISTSIZE=900001
      HISTFILESIZE=$HISTSIZE
      SAVEHIST=$HISTSIZE

      autoload -U promptinit
      promptinit

      error='%(?..%F{red}%?%f )'

      case $UID in
        0)
          username='%F{red}root%f '
          ;;
        1337)
          username=""
          ;;
        *)
          username='%F{blue}%n%f '
          ;;
      esac

      if test -n "$SSH_CLIENT"; then
        PROMPT="$error$username@%F{magenta}%M%f %~ "
      else
        PROMPT="$error$username%~ "
      fi
    '';
  };
  users.users.mainUser.shell = "/run/current-system/sw/bin/zsh";
  users.users.root.shell = "/run/current-system/sw/bin/zsh";
}

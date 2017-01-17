{ config, lib, pkgs, ... }:
{
  programs.zsh = {
    enable = true;
    shellInit = ''
      #disable config wizard
      zsh-newuser-install() { :; }
    '';
    interactiveShellInit = ''
      #unsetopt nomatch
      setopt autocd extendedglob
      bindkey -e
      zstyle :compinstall filename '/home/lass/.zshrc'

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

      #completion magic
      fpath=(~/.zsh/completions $fpath)
      autoload -Uz compinit
      compinit
      zstyle ':completion:*' menu select

      #enable automatic rehashing of $PATH
      zstyle ':completion:*' rehash true


      #eval $( dircolors -b ~/.LS_COLORS )

      # export MANPAGER='sed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g" | vim -R -c "set ft=man nonu nomod nolist" -'

      #beautiful colors
      alias ls='ls --color'
      zstyle ':completion:*:default' list-colors ''${(s.:.)LS_COLORS}

      #emacs bindings
      bindkey "[7~" beginning-of-line
      bindkey "[8~" end-of-line
      bindkey "Oc" emacs-forward-word
      bindkey "Od" emacs-backward-word

      #aliases
      alias ll='ls -l'
      alias la='ls -la'
      alias pinginet='ping 8.8.8.8'
      alias du='du -hd1'
      alias qiv="qiv -f -m"
      alias zshres="source ~/.zshrc"

      #fancy window title magic
      case $TERM in
        (*xterm* | *rxvt*)

          # Write some info to terminal title.
          # This is seen when the shell prompts for input.
          function precmd {
            print -Pn "\e]0;%(1j,%j job%(2j|s|); ,)%~\a"
          }
          # Write command and args to terminal title.
          # This is seen while the shell waits for a command to complete.
          function preexec {
            printf "\033]0;%s\a" "$1"
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
}

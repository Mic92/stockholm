{ config, pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    (vim_configurable.customize {
      name = "vim";

    vimrcConfig.customRC = ''
      set nocompatible
      set t_Co=16
      syntax on
      " TODO autoload colorscheme file
      set background=dark
      colorscheme solarized
      filetype off
      filetype plugin indent on

      imap <F1> <nop>

      set mouse=a
      set ruler
      set showmatch
      set backspace=2
      set visualbell
      set encoding=utf8
      set showcmd
      set wildmenu

      set title
      set titleold=
      set titlestring=%t%(\ %M%)%(\ (%{expand(\"%:p:h\")})%)%(\ %a%)\ -\ %{v:servername}

      set autoindent

      set ttyfast

      set pastetoggle=<INS>


      " Force Saving Files that Require Root Permission
      command! W silent w !sudo tee "%" >/dev/null

      nnoremap <C-c> :q<Return>
      vnoremap < <gv
      vnoremap > >gv

      "Tabwidth
      set ts=2 sts=2 sw=2 et
      autocmd BufRead *.js,*.json set ts=2 sts=2 sw=2 et
      autocmd BufRead *.hs set ts=4 sts=4 sw=4 et

      " create Backup/tmp/undo dirs
      function! InitBackupDir()
        let l:parent = $HOME . '/.vim/'
        let l:backup = l:parent . 'backups/'
        let l:tmpdir = l:parent . 'tmp/'
        let l:undodi = l:parent . 'undo/'

        if !isdirectory(l:parent)
          call mkdir(l:parent)
        endif
        if !isdirectory(l:backup)
          call mkdir(l:backup)
        endif
        if !isdirectory(l:tmpdir)
          call mkdir(l:tmpdir)
        endif
        if !isdirectory(l:undodi)
          call mkdir(l:undodi)
        endif
      endfunction
      call InitBackupDir()

      " Backups & Files
      set backup
      set backupdir=~/.vim/backups
      set directory=~/.vim/tmp//
      set viminfo='20,<1000,s100,h,n~/.vim/tmp/info
      set undodir=$HOME/.vim/undo
      set undofile
    '';

      vimrcConfig.vam.knownPlugins = vimPlugins;
      vimrcConfig.vam.pluginDictionaries = [
        { name = "Gundo"; }
        { name = "commentary"; }
        { name = "vim-addon-nix"; }
        { name = "colors-solarized"; }
      ];
    })
  ];
}

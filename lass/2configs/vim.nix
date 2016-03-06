{ config, pkgs, ... }:

let
  customPlugins = {
    mustang2 = pkgs.vimUtils.buildVimPlugin {
      name = "Mustang2";
      src = pkgs.fetchFromGitHub {
        owner = "croaker";
        repo = "mustang-vim";
        rev = "6533d7d21bf27cae94d9c2caa575f627f003dfd5";
        sha256 = "0zlmcrr04j3dkiivrhqi90f618lmnnnpvbz1b9msfs78cmgw9w67";
      };
    };
    unimpaired = pkgs.vimUtils.buildVimPlugin {
      name = "unimpaired-vim";
      src = pkgs.fetchFromGitHub {
        owner = "tpope";
        repo = "vim-unimpaired";
        rev = "11dc568dbfd7a56866a4354c737515769f08e9fe";
        sha256 = "1an941j5ckas8l3vkfhchdzjwcray16229rhv3a1d4pbxifwshi8";
      };
    };
  };

in {

  environment.systemPackages = [
    (pkgs.vim_configurable.customize {
      name = "vim";

    vimrcConfig.customRC = ''
      set nocompatible
      set t_Co=16
      syntax on
      " TODO autoload colorscheme file
      set background=dark
      colorscheme mustang
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

      nmap <esc>q :buffer 

      "Tabwidth
      set ts=2 sts=2 sw=2 et

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

      " highlight whitespaces
      highlight ExtraWhitespace ctermbg=red guibg=red
      match ExtraWhitespace /\s\+$/
      autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
      autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
      autocmd InsertLeave * match ExtraWhitespace /\s\+$/
      autocmd BufWinLeave * call clearmatches()

      "ft specific stuff
      autocmd BufRead *.js,*.json set ts=2 sts=2 sw=2 et
      autocmd BufRead *.hs set ts=4 sts=4 sw=4 et

      "esc timeout
      set timeoutlen=1000 ttimeoutlen=0

      "foldfunctions
      inoremap <F9> <C-O>za
      nnoremap <F9> za
      onoremap <F9> <C-C>za
      vnoremap <F9> zf
    '';

      vimrcConfig.vam.knownPlugins = pkgs.vimPlugins // customPlugins;
      vimrcConfig.vam.pluginDictionaries = [
        { names = [
          "Gundo"
          "commentary"
          "mustang2"
          "extradite"
          "fugitive"
          "unimpaired"
        ]; }
        { names = [ "vim-addon-nix" ]; ft_regex = "^nix\$"; }
      ];

    })
  ];
}

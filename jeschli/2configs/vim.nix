{ config, pkgs, ... }:

let
  customPlugins.vim-javascript = pkgs.vimUtils.buildVimPlugin {
    name = "vim-javascript";
    src = pkgs.fetchFromGitHub {
      owner = "pangloss";
      repo = "vim-javascript";
      rev = "1.2.5.1";
      sha256 = "08l7ricd3j5h2bj9i566byh39v9n5wj5mj75f2c8a5dsc732b2k7";
    };
  };
   customPlugins.vim-jsx = pkgs.vimUtils.buildVimPlugin {
     name = "vim-jsx";
     src = pkgs.fetchFromGitHub {
       owner = "mxw";
       repo = "vim-jsx";
       rev = "5b968dfa512c57c38ad7fe420f3e8ab75a73949a";
       sha256 = "1z3yhhbmbzfw68qjzyvpbmlyv2a1p814sy5q2knn04kcl30vx94a"; 
     };
   };
in {
# {
  environment.systemPackages = [
    (pkgs.vim_configurable.customize {
      name = "vim";

    vimrcConfig.customRC = ''
  set nocompatible 

	:imap jk <Esc>
	:vmap v v
	:map gr :GoRun<Enter>
	:nnoremap <S-TAB> :bnext<CR>
	:nnoremap <C-TAB> <c-w><c-w>
  :map nf :NERDTreeToggle<CR>
	set autowrite
	set number
	set ruler
  set path+=** 
  set wildmenu

	noremap x "_x
	set clipboard=unnamedplus

  let g:jsx_ext_required = 0

	let g:go_list_type = "quickfix"
	let g:go_test_timeout = '10s'
	let g:go_fmt_command = "goimports"
	let g:go_snippet_case_type = "camelcase"
	let g:go_highlight_types = 1
	let g:go_highlight_fields = 1
	let g:go_highlight_functions = 1
	let g:go_highlight_methods = 1
  let g:go_highlight_extra_types = 1
  autocmd BufNewFile,BufRead *.go setlocal noexpandtab tabstop=4 shiftwidth=4 
  let g:rehash256 = 1
  let g:molokai_original = 1
  colorscheme molokai
	let g:go_metalinter_enabled = ['vet', 'golint', 'errcheck']
	let g:go_metalinter_autosave = 1
	" let g:go_metalinter_autosave_enabled = ['vet', 'golint']
	" let g:go_def_mode = 'godef'
	" let g:go_decls_includes = "func,type"


	" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
	let g:UltiSnipsExpandTrigger="<c-e>"
	let g:UltiSnipsJumpForwardTrigger="<c-t>"
	let g:UltiSnipsJumpBackwardTrigger="<c-q>"

	" If you want :UltiSnipsEdit to split your window.
	let g:UltiSnipsEditSplit="vertical"

	if has('persistent_undo')      "check if your vim version supports it
	set undofile                 "turn on the feature  
	set undodir=$HOME/.vim/undo  "directory where the undo files will be stored
	endif     
        '';

       vimrcConfig.vam.knownPlugins = pkgs.vimPlugins // customPlugins;
       vimrcConfig.vam.pluginDictionaries = [
         { names = [ "undotree" "molokai" "Syntastic" "ctrlp" "surround" "snipmate" "nerdtree" "easymotion"]; } 
         { names = [ "vim-addon-nix" ]; ft_regex = "^nix\$"; }
         { names = [ "vim-go" ]; ft_regex = "^go\$"; } # wanted: nsf/gocode
         { names = [ "vim-javascript" ]; ft_regex = "^js\$"; }
         { names = [ "vim-jsx" ]; ft_regex = "^js\$"; }
       ];
    })
  ];
}

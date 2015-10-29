.PHONY: ghci
ghci: shell.nix
	nix-shell --command 'exec ghci -Wall'

shell.nix: xmonad.cabal
	cabal2nix --shell . > $@

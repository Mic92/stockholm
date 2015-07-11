ifndef system
$(error unbound variable: system)
else
include 0make/tv/$(system).makefile
.ONESHELL:
.SHELLFLAGS := -eufc
.PHONY: deploy
deploy:;@
	system_name=$(system)
	deploy_host=$(deploy_host)
	nixpkgs_url=$(nixpkgs_url)
	nixpkgs_rev=$(nixpkgs_rev)
	secrets_dir=$(secrets_dir)

	prepush(){(
		dst=$$1
		src=$$2
		rsync \
			--exclude .git \
			--exclude .graveyard \
			--exclude old \
			--rsync-path="mkdir -p \"$$dst\" && rsync" \
			--usermap=\*:0 \
			--groupmap=\*:0 \
			--delete-excluded \
			-vrLptgoD \
			"$$src/" "$$deploy_host:$$dst"
	)}

	prepush /root/src/shitment "$$PWD"
	prepush /root/src/secrets "$$secrets_dir"

	ssh -S none "$$deploy_host" -T env \
			nixpkgs_url="$$nixpkgs_url" \
			nixpkgs_rev="$$nixpkgs_rev" \
			system_name="$$system_name" \
		sh -euf \
	<<-\EOF
		prefetch(){(
			dst=$$1
			url=$$2
			rev=$$3
			mkdir -p "$$dst"
			cd "$$dst"
			if ! test -e .git; then
				git init
			fi
			if ! cur_url=$$(git config remote.origin.url 2>/dev/null); then
				git remote add origin "$$url"
			elif test "$$cur_url" != "$$url"; then
				git remote set-url origin "$$url"
			fi
			if test "$$(git rev-parse --verify HEAD 2>/dev/null)" != "$$rev"; then
				git fetch origin
				git checkout "$$rev" -- .
				git checkout -q "$$rev"
				git submodule init
				git submodule update
			fi
			git clean -dxf
		)}

		prefetch /root/src/nixpkgs "$$nixpkgs_url" "$$nixpkgs_rev"

		echo build system...
		NIXOS_CONFIG=/root/src/shitment/1systems/tv/$$system_name.nix \
		NIX_PATH=src \
			nix-build -Q -A system '<nixpkgs/nixos>'

		result/bin/switch-to-configuration switch
	EOF
endif

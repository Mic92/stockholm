#
# usage:
#		make system=foo
#		make systems='foo bar'
#		make eval system=foo get=config.networking.extraHosts
#

.ONESHELL:
.SHELLFLAGS := -eufc

ifdef systems
$(systems):
	parallel \
		--line-buffer \
		-j0 \
		--no-notice \
		--tagstring {} \
		-q make systems= system={} ::: $(systems)
else ifdef system
include 0make/$(LOGNAME)/$(system).makefile
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

	prepush /root/src/stockholm "$$PWD"
	prepush /root/src/secrets "$$secrets_dir"

	ssh -S none "$$deploy_host" -T env \
			nixpkgs_url="$$nixpkgs_url" \
			nixpkgs_rev="$$nixpkgs_rev" \
			system_name="$$system_name" \
			user_name="$$LOGNAME" \
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
		NIX_PATH=/root/src \
		nix-build \
			-Q \
			-A system \
			'<stockholm>' \
			--argstr user-name "$$user_name" \
			--argstr system-name "$$system_name"

		result/bin/switch-to-configuration switch
	EOF

.PHONY: eval
eval:
	@
	NIX_PATH=stockholm=$$PWD:$$NIX_PATH \
	nix-instantiate \
		--json \
		--eval \
		--strict \
		-A "$$get" \
		'<stockholm>' \
		--argstr user-name "$$LOGNAME" \
		--argstr system-name "$$system" \
		| jq -r .
else
$(error unbound variable: system[s])
endif

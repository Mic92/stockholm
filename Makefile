.ONESHELL:
.SHELLFLAGS := -eufc

ifndef system
$(error unbound variable: system)
endif

export target_host ?= $(system)
export target_user ?= root
export target_path ?= /var/src

# usage: make deploy system=foo [target_host=bar]
.PHONY: deploy
deploy: populate ;@set -x
	ssh "$$target_user@$$target_host" nixos-rebuild switch -I "$$target_path"

# usage: make populate system=foo [target_host=bar]
.PHONY: populate
populate:;@
	result=$$(make -s eval get=config.krebs.build.populate filter=json)
	echo "$$result" | sh

# usage: make eval system=foo get=config.krebs.build [LOGNAME=tv] [filter=json]
.PHONY: eval
eval:;@
ifeq ($(filter),json)
	extraArgs='--json --strict'
	filter() { echo "$$1" | jq -r .; }
else
	filter() { echo "$$1"; }
endif
	result=$$(nix-instantiate \
		$${extraArgs-} \
		--show-trace \
		--readonly-mode \
		--eval \
		-A "$$get" \
		--arg configuration "<stockholm/$$LOGNAME/1systems/$$system.nix>")
	filter "$$result"

## usage: make install system=foo target=
#.PHONY: install
#install: ssh = ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null
#install:;@set -x
#	$(ssh) "$$target_user@$$target_host" \
#		env target_path=/var/src \
#			sh -s prepare < krebs/4lib/infest/prepare.sh
#	make -s populate target_path=/mnt"$$target_path"
#	$(ssh) "$$target_user@$$target_host" \
#		env NIXOS_CONFIG=/var/src/nixos-config \
#			nixos-install

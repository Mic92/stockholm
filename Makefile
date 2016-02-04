.ONESHELL:
.SHELLFLAGS := -eufc

ifndef system
$(error unbound variable: system)
endif

export target_host ?= $(system)
export target_user ?= root
export target_path ?= /var/src

evaluate = \
	nix-instantiate \
		--arg configuration "./$$LOGNAME/1systems/$$system.nix" \
		--eval \
		--readonly-mode \
		--show-trace \
		$(1)

execute = $(call evaluate,-A config.krebs.build.$(1) --json) | jq -r . | sh

# usage: make deploy system=foo [target_host=bar]
deploy:
	$(call execute,populate)
	ssh "$$target_user@$$target_host" nixos-rebuild switch -I "$$target_path"

# usage: make LOGNAME=shared system=wolf eval.config.krebs.build.host.name
eval eval.:;@$(call evaluate)
eval.%:;@$(call evaluate,-A $*)

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

#
# usage:
#		make infest system=foo [target=bar]
#		make [deploy] system=foo [target=bar]
#		make [deploy] systems='foo bar'
#		make eval get=users.tv.wu.config.time.timeZone [filter=json]
#

.ONESHELL:
.SHELLFLAGS := -eufc

ifdef systems
$(systems):
	@
	unset target
	parallel \
		--line-buffer \
		-j0 \
		--no-notice \
		--tagstring {} \
		-q make -s systems= system={} ::: $(systems)
else ifdef system
.PHONY: deploy infest
deploy infest:;@
	export get=krebs.$@
	export filter=json
	script=$$(make -s eval)
	echo "$$script" | sh

.PHONY: eval
eval:
	@
ifeq ($(filter),json)
	extraArgs='--json --strict'
	filter() { jq -r .; }
else
	filter() { cat; }
endif
	result=$$(nix-instantiate \
		$${extraArgs-} \
		--eval \
		-A "$$get" \
		-I stockholm="$$PWD" \
		'<stockholm>' \
		--argstr current-host-name "$$HOSTNAME" \
		--argstr current-user-name "$$LOGNAME" \
		$${system+--argstr system "$$system"} \
		$${target+--argstr target "$$target"})
	echo "$$result" | filter

export target_host ?= $(system)
export target_user ?= root
export target_path ?= /var/src

# usage: make populate system=foo [target_host=bar]
.PHONY: populate
populate: export lib = \
	let nlib = import <nixpkgs/lib>; in \
	nlib // import krebs/4lib { lib = nlib; } // builtins
populate: export source = \
	with builtins; \
	with (import ./. {}).users.$${getEnv "LOGNAME"}.$${getEnv "system"}; \
	assert config.krebs.build.source-version == 2; \
	config.krebs.build.source
populate:;@
	result=$$(nix-instantiate \
			--eval \
			--json \
			--arg lib "$$lib" \
			--arg source "$$source" \
			--argstr target-user "$$target_user" \
			--argstr target-host "$$target_host" \
			--argstr target-path "$$target_path" \
			-A populate \
			krebs/v2)
	script=$$(echo "$$result" | jq -r .)
	echo "$$script" | sh

# usage: make rebuild system=foo [target_host=bar] [operation=switch]
.PHONY: rebuild
rebuild: populate ;@set -x
	ssh "$$target_user@$$target_host" \
		nixos-rebuild "$${operation-switch}" -I "$$target_path"

else
$(error unbound variable: system[s])
endif

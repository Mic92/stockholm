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
		--argstr current-date "$$(date -Is)" \
		--argstr current-host-name "$$HOSTNAME" \
		--argstr current-user-name "$$LOGNAME" \
		$${system+--argstr system "$$system"} \
		$${target+--argstr target "$$target"})
	echo "$$result" | filter

ifndef target
export target = $(system)
endif

# usage: make populate system=foo [target=bar]
.PHONY: populate
populate: export source = \
	with (import ~/stockholm {}).users.$(LOGNAME).$(system).config.krebs.build; \
	assert source-version == 2; \
	source
populate:;@
	result=$$(nix-instantiate \
			--eval \
			--json \
			--arg source "$$source" \
			--argstr target-host "$$target" \
			--argstr target-path /var/src \
			-A populate \
			krebs/v2)
	script=$$(echo "$$result" | jq -r .)
	echo "$$script" | sh

# usage: make rebuild system=foo [target=bar] [operation=switch]
.PHONY: rebuild
rebuild: populate ;@set -x
	ssh root@"$$target" nixos-rebuild "$${operation-switch}" -I /var/src

else
$(error unbound variable: system[s])
endif

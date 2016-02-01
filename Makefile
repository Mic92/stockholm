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

.PHONY: deploy2
ifdef target
deploy2: export target-host = $(target)
else
deploy2: export target-host = $(system)
endif
deploy2:;@
	target=$${target-$$system}
	result=$$(nix-instantiate \
			--json \
			--eval \
			krebs/populate.nix \
			--arg source 'with (import ~/stockholm {}).users.$(LOGNAME).$(system).config.krebs.build; assert source-version == 2; source' \
			--argstr target-host "$$target" \
			--argstr target-path /var/src)
	script=$$(echo "$$result" | jq -r .)
	echo "$$script" | sh
	ssh root@$$target nixos-rebuild switch -I /var/src

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

else
$(error unbound variable: system[s])
endif

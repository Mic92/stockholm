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
	make -s eval | sh

.PHONY: eval
eval:
	@
ifeq ($(filter),json)
	extraArgs='--json --strict'
	filter() { jq -r .; }
else
	filter() { cat; }
endif
	nix-instantiate \
		$${extraArgs-} \
		--eval \
		-A "$$get" \
		-I stockholm="$$PWD" \
		'<stockholm>' \
		--argstr current-date "$$(date -Is)" \
		--argstr current-host-name "$$HOSTNAME" \
		--argstr current-user-name "$$LOGNAME" \
		$${system+--argstr system "$$system"} \
		$${target+--argstr target "$$target"} \
		| filter
else
$(error unbound variable: system[s])
endif

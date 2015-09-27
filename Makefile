#
# usage:
#		make system=foo
#		make systems='foo bar'
#		make eval get=tv.wu.config.time.timeZone [filter=json]
#

.ONESHELL:
.SHELLFLAGS := -eufc

ifdef systems
$(systems):
	@
	parallel \
		--line-buffer \
		-j0 \
		--no-notice \
		--tagstring {} \
		-q make -s systems= system={} ::: $(systems)
else ifdef system
.PHONY: deploy infest
deploy infest:;@
	export get=$$LOGNAME.${system}.config.krebs.build.scripts.$@
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
	NIX_PATH=stockholm=$$PWD:$$NIX_PATH \
	nix-instantiate \
		$${extraArgs-} \
		--eval \
		-A "$$get" \
		'<stockholm>' \
		--argstr user-name "$$LOGNAME" \
		--argstr host-name "$$HOSTNAME" \
		| filter
else
$(error unbound variable: system[s])
endif

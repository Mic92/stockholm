#
# usage:
#		make system=foo
#		make systems='foo bar'
#		make eval system=foo get=config.networking.extraHosts [filter=json]
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
.PHONY: deploy
deploy:;@
	make -s eval system=$(system) get=config.krebs.build.script filter=json | sh

.PHONY: infest
infest:;@
	make -s eval system=$(system) get=config.krebs.build.infest filter=json | sh

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
		--argstr system-name "$$system" \
		| filter
else
$(error unbound variable: system[s])
endif

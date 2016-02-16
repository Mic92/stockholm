ifndef system
$(error unbound variable: system)
endif

# target = [target_user@]target_host[:target_port][/target_path]
ifdef target
_target_user != echo $(target) | sed -n 's/@.*//p'
_target_path != echo $(target) | sed -n 's/^[^/]*//p'
_target_port != echo $(target) | sed -En 's|^.*:([^/]*)(/.*)?$$|\1|p'
_target_host != echo $(target) | sed -En 's/^(.*@)?([^:/]*).*/\2/p'
ifneq ($(_target_host),)
$(if $(target_host),$(error cannot define both, target_host and host in target))
target_host ?= $(_target_host)
endif
ifneq ($(_target_user),)
$(if $(target_user),$(error cannot define both, target_user and user in target))
target_user ?= $(_target_user)
endif
ifneq ($(_target_port),)
$(if $(target_port),$(error cannot define both, target_port and port in target))
target_port ?= $(_target_port)
endif
ifneq ($(_target_path),)
$(if $(target_path),$(error cannot define both, target_path and path in target))
target_path ?= $(_target_path)
endif
endif

export target_host ?= $(system)
export target_user ?= root
export target_port ?= 22
export target_path ?= /var/src

evaluate = \
	nix-instantiate \
		--eval \
		--readonly-mode \
		--show-trace \
		-I nixos-config=./$(LOGNAME)/1systems/$(system).nix \
		-I stockholm=. \
		$(1)

execute = \
	result=$$($(call evaluate,-A config.krebs.build.$(1) --json)) && \
	script=$$(echo "$$result" | jq -r .) && \
	echo "$$script" | sh

# usage: make deploy system=foo [target_host=bar]
deploy:
	$(call execute,populate)
	ssh $(target_user)@$(target_host) -p $(target_port) \
		nixos-rebuild switch --show-trace -I $(target_path)

# usage: make LOGNAME=shared system=wolf eval.config.krebs.build.host.name
eval eval.:;@$(call evaluate)
eval.%:;@$(call evaluate,-A $*)

# usage: make install system=foo [target_host=bar]
install: ssh ?= ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null
install:
	$(ssh) $(target_user)@$(target_host) -p $(target_port) \
		env target_path=$(target_path) \
			sh -s prepare < krebs/4lib/infest/prepare.sh
	target_path=/mnt$(target_path) $(call execute,populate)
	$(ssh) $(target_user)@$(target_host) -p $(target_port) \
		env NIXOS_CONFIG=$(target_path)/nixos-config \
			nixos-install

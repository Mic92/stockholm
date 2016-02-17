stockholm ?= .

ifndef nixos-config
$(if $(system),,$(error unbound variable: system))
nixos-config = ./$(LOGNAME)/1systems/$(system).nix
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

$(if $(target_host),,$(error unbound variable: target_host))
$(if $(target_user),,$(error unbound variable: target_user))
$(if $(target_port),,$(error unbound variable: target_port))
$(if $(target_path),,$(error unbound variable: target_path))

evaluate = \
	nix-instantiate \
		--eval \
		--readonly-mode \
		--show-trace \
		-I nixos-config=$(nixos-config) \
		-I stockholm=$(stockholm) \
		$(1)

execute = \
	result=$$($(call evaluate,-A config.krebs.build.$(1) --json)) && \
	script=$$(echo "$$result" | jq -r .) && \
	echo "$$script" | sh

# usage: make deploy system=foo [target_host=bar]
deploy: ssh ?= ssh
deploy:
	$(call execute,populate)
	$(ssh) $(target_user)@$(target_host) -p $(target_port) \
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

# usage: make test system=foo [target=bar] [method={eval,build}]
method ?= eval
ifeq ($(method),build)
test: command = nix-build --no-out-link
else
ifeq ($(method),eval)
test: command ?= nix-instantiate --eval --json --readonly-mode --strict
else
$(error bad method: $(method))
endif
endif
test: ssh ?= ssh
test:
	$(call execute,populate)
	$(ssh) $(target_user)@$(target_host) -p $(target_port) \
		$(command) --show-trace -I $(target_path) \
			-A config.system.build.toplevel $(target_path)/stockholm

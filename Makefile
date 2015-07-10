all:;@exit 23

tv-cluster := cd mkdir nomic rmdir wu
deploy-cd:; ./deploy cd
deploy-mkdir:; ./deploy mkdir
deploy-nomic:; ./deploy nomic root@nomic-local
deploy-rmdir:; ./deploy rmdir
deploy-wu:; ./deploy wu root@localhost

ifndef cluster
cluster := $(LOGNAME)
endif
hosts := $($(cluster)-cluster)
ifeq ($(hosts),)
$(error bad cluster: $(cluster))
else
.ONESHELL:

.PHONY: deploy $(addprefix deploy-,$(hosts))
deploy:
	exec parallel \
		-j 0 \
		--no-notice \
		--rpl '{u} s/^.* deploy-(.*)/\1/' \
		--tagstring '{u}' \
		--line-buffer \
		$(MAKE) deploy-{} ::: $(hosts)

.PHONY: rotate-consul-encrypt
rotate-consul-encrypt:
	umask 0377
	mkencrypt() { dd status=none if=/dev/random bs=1 count=16 | base64; }
	json=$$(printf '{"encrypt":"%s"}\n' $$(mkencrypt))
	cmd='
		f=secrets/{}/rsync/etc/consul/encrypt.json
		rm -f "$$f"
		echo "$$json" > "$$f"
	'
	export json
	exec parallel \
		-j 0 \
		--no-notice \
		--rpl '{u} s/^.* deploy-(.*)/\1/' \
		--tagstring '{u}' \
		--line-buffer \
		--quote \
		sh -eufc "$$cmd" ::: $(hosts)
endif

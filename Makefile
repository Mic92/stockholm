all: deploy

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
.PHONY: deploy $(addprefix deploy-,$(hosts))
deploy:
	exec parallel \
		-j 0 \
		--no-notice \
		--rpl '{u} s/^.* deploy-(.*)/\1/' \
		--tagstring '{u}' \
		--line-buffer \
		$(MAKE) deploy-{} ::: $(hosts)
endif

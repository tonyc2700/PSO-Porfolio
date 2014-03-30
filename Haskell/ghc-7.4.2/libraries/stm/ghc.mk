libraries/stm_PACKAGE = stm
libraries/stm_dist-install_GROUP = libraries
$(if $(filter stm,$(PKGS_THAT_BUILD_WITH_STAGE0)),$(eval $(call build-package,libraries/stm,dist-boot,0)))
$(eval $(call build-package,libraries/stm,dist-install,$(if $(filter stm,$(PKGS_THAT_BUILD_WITH_STAGE2)),2,1)))

libraries/edenmodules_PACKAGE = edenmodules
libraries/edenmodules_dist-install_GROUP = libraries
$(if $(filter edenmodules,$(PKGS_THAT_BUILD_WITH_STAGE0)),$(eval $(call build-package,libraries/edenmodules,dist-boot,0)))
$(eval $(call build-package,libraries/edenmodules,dist-install,$(if $(filter edenmodules,$(PKGS_THAT_BUILD_WITH_STAGE2)),2,1)))

libraries/parallel_PACKAGE = parallel
libraries/parallel_dist-install_GROUP = libraries
$(if $(filter parallel,$(PKGS_THAT_BUILD_WITH_STAGE0)),$(eval $(call build-package,libraries/parallel,dist-boot,0)))
$(eval $(call build-package,libraries/parallel,dist-install,$(if $(filter parallel,$(PKGS_THAT_BUILD_WITH_STAGE2)),2,1)))


utils/compare_sizes_USES_CABAL = YES
utils/compare_sizes_PACKAGE = compareSizes
utils/compare_sizes_MODULES = Main
utils/compare_sizes_dist-install_PROG = compareSizes$(exeext)

$(eval $(call build-prog,utils/compare_sizes,dist-install,1))


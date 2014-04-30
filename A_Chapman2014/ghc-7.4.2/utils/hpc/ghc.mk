# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

utils/hpc_dist-install_MODULES = Main HpcCombine HpcDraft HpcFlags HpcLexer \
			 HpcMarkup HpcOverlay HpcParser HpcReport \
			 HpcShowTix HpcUtils
utils/hpc_dist-install_HC_OPTS = -cpp -package hpc
utils/hpc_dist-install_INSTALL = YES
utils/hpc_dist-install_PROG    = hpc$(exeext)
$(eval $(call build-prog,utils/hpc,dist-install,1))

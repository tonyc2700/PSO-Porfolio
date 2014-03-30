libraries/hpc_dist-install_VERSION = 0.5.1.1
libraries/hpc_dist-install_MODULES = Trace.Hpc.Util Trace.Hpc.Mix Trace.Hpc.Tix Trace.Hpc.Reflect
libraries/hpc_dist-install_HIDDEN_MODULES = 
libraries/hpc_dist-install_SYNOPSIS =Code Coverage Library for Haskell
libraries/hpc_dist-install_HS_SRC_DIRS = .
libraries/hpc_dist-install_DEPS = base-4.5.1.0 containers-0.4.2.1 directory-1.1.0.2 old-time-1.1.0.0
libraries/hpc_dist-install_DEP_NAMES = base containers directory old-time
libraries/hpc_dist-install_INCLUDE_DIRS = 
libraries/hpc_dist-install_INCLUDES = 
libraries/hpc_dist-install_INSTALL_INCLUDES = 
libraries/hpc_dist-install_EXTRA_LIBRARIES = 
libraries/hpc_dist-install_EXTRA_LIBDIRS = 
libraries/hpc_dist-install_C_SRCS  = 
libraries/hpc_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard libraries/hpc/cbits/*.cmm)))
libraries/hpc_dist-install_DATA_FILES = 
libraries/hpc_dist-install_HC_OPTS = -Wall -XHaskell98 -XCPP
libraries/hpc_dist-install_CC_OPTS = 
libraries/hpc_dist-install_CPP_OPTS = 
libraries/hpc_dist-install_LD_OPTS = 
libraries/hpc_dist-install_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/directory/include' '/home/dieterle/ghc-eden/libraries/unix/include' '/home/dieterle/ghc-eden/libraries/old-time/include' '/home/dieterle/ghc-eden/libraries/containers/include' '/home/dieterle/ghc-eden/libraries/bytestring/include' '/home/dieterle/ghc-eden/libraries/array/include' '/home/dieterle/ghc-eden/libraries/base/include' '/home/dieterle/ghc-eden/rts/dist/build' '/home/dieterle/ghc-eden/includes' '/home/dieterle/ghc-eden/includes/dist-ghcconstants/header' '/home/dieterle/ghc-eden/includes/dist-derivedconstants/header'
libraries/hpc_dist-install_DEP_CC_OPTS = 
libraries/hpc_dist-install_DEP_LIB_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/directory/dist-install/build' '/home/dieterle/ghc-eden/libraries/unix/dist-install/build' '/home/dieterle/ghc-eden/libraries/old-time/dist-install/build' '/home/dieterle/ghc-eden/libraries/old-locale/dist-install/build' '/home/dieterle/ghc-eden/libraries/filepath/dist-install/build' '/home/dieterle/ghc-eden/libraries/containers/dist-install/build' '/home/dieterle/ghc-eden/libraries/deepseq/dist-install/build' '/home/dieterle/ghc-eden/libraries/bytestring/dist-install/build' '/home/dieterle/ghc-eden/libraries/array/dist-install/build' '/home/dieterle/ghc-eden/libraries/base/dist-install/build' '/home/dieterle/ghc-eden/libraries/integer-gmp/dist-install/build' '/home/dieterle/ghc-eden/libraries/ghc-prim/dist-install/build' '/home/dieterle/ghc-eden/rts/dist/build'
libraries/hpc_dist-install_DEP_EXTRA_LIBS = rt util dl pthread gmp m rt dl
libraries/hpc_dist-install_DEP_LD_OPTS = 
libraries/hpc_dist-install_BUILD_GHCI_LIB = YES

$(eval $(libraries/hpc_PACKAGE_MAGIC))

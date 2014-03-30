libraries/parallel_dist-install_VERSION = 3.2.0.3
libraries/parallel_dist-install_MODULES = Control.Seq Control.Parallel Control.Parallel.Strategies
libraries/parallel_dist-install_HIDDEN_MODULES = 
libraries/parallel_dist-install_SYNOPSIS =Parallel programming library
libraries/parallel_dist-install_HS_SRC_DIRS = .
libraries/parallel_dist-install_DEPS = array-0.4.0.0 base-4.5.1.0 containers-0.4.2.1 deepseq-1.3.0.0
libraries/parallel_dist-install_DEP_NAMES = array base containers deepseq
libraries/parallel_dist-install_INCLUDE_DIRS = 
libraries/parallel_dist-install_INCLUDES = 
libraries/parallel_dist-install_INSTALL_INCLUDES = 
libraries/parallel_dist-install_EXTRA_LIBRARIES = 
libraries/parallel_dist-install_EXTRA_LIBDIRS = 
libraries/parallel_dist-install_C_SRCS  = 
libraries/parallel_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard libraries/parallel/cbits/*.cmm)))
libraries/parallel_dist-install_DATA_FILES = 
libraries/parallel_dist-install_HC_OPTS = -feager-blackholing -Wall -XHaskell98 -XCPP -XBangPatterns
libraries/parallel_dist-install_CC_OPTS = 
libraries/parallel_dist-install_CPP_OPTS = 
libraries/parallel_dist-install_LD_OPTS = 
libraries/parallel_dist-install_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/containers/include' '/home/dieterle/ghc-eden/libraries/array/include' '/home/dieterle/ghc-eden/libraries/base/include' '/home/dieterle/ghc-eden/rts/dist/build' '/home/dieterle/ghc-eden/includes' '/home/dieterle/ghc-eden/includes/dist-ghcconstants/header' '/home/dieterle/ghc-eden/includes/dist-derivedconstants/header'
libraries/parallel_dist-install_DEP_CC_OPTS = 
libraries/parallel_dist-install_DEP_LIB_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/containers/dist-install/build' '/home/dieterle/ghc-eden/libraries/deepseq/dist-install/build' '/home/dieterle/ghc-eden/libraries/array/dist-install/build' '/home/dieterle/ghc-eden/libraries/base/dist-install/build' '/home/dieterle/ghc-eden/libraries/integer-gmp/dist-install/build' '/home/dieterle/ghc-eden/libraries/ghc-prim/dist-install/build' '/home/dieterle/ghc-eden/rts/dist/build'
libraries/parallel_dist-install_DEP_EXTRA_LIBS = gmp m rt dl
libraries/parallel_dist-install_DEP_LD_OPTS = 
libraries/parallel_dist-install_BUILD_GHCI_LIB = YES

$(eval $(libraries/parallel_PACKAGE_MAGIC))

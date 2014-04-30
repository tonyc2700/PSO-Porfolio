libraries/containers_dist-install_VERSION = 0.4.2.1
libraries/containers_dist-install_MODULES = Data.Graph Data.Sequence Data.Tree Data.IntMap Data.IntSet Data.Map Data.Set
libraries/containers_dist-install_HIDDEN_MODULES = 
libraries/containers_dist-install_SYNOPSIS =Assorted concrete container types
libraries/containers_dist-install_HS_SRC_DIRS = .
libraries/containers_dist-install_DEPS = array-0.4.0.0 base-4.5.1.0 deepseq-1.3.0.0
libraries/containers_dist-install_DEP_NAMES = array base deepseq
libraries/containers_dist-install_INCLUDE_DIRS = include
libraries/containers_dist-install_INCLUDES = 
libraries/containers_dist-install_INSTALL_INCLUDES = 
libraries/containers_dist-install_EXTRA_LIBRARIES = 
libraries/containers_dist-install_EXTRA_LIBDIRS = 
libraries/containers_dist-install_C_SRCS  = 
libraries/containers_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard libraries/containers/cbits/*.cmm)))
libraries/containers_dist-install_DATA_FILES = 
libraries/containers_dist-install_HC_OPTS = -fregs-graph -O2 -XHaskell98 -XDeriveDataTypeable -XStandaloneDeriving -XMagicHash -XRank2Types -XCPP
libraries/containers_dist-install_CC_OPTS = 
libraries/containers_dist-install_CPP_OPTS = 
libraries/containers_dist-install_LD_OPTS = 
libraries/containers_dist-install_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/array/include' '/home/dieterle/ghc-eden/libraries/base/include' '/home/dieterle/ghc-eden/rts/dist/build' '/home/dieterle/ghc-eden/includes' '/home/dieterle/ghc-eden/includes/dist-ghcconstants/header' '/home/dieterle/ghc-eden/includes/dist-derivedconstants/header'
libraries/containers_dist-install_DEP_CC_OPTS = 
libraries/containers_dist-install_DEP_LIB_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/deepseq/dist-install/build' '/home/dieterle/ghc-eden/libraries/array/dist-install/build' '/home/dieterle/ghc-eden/libraries/base/dist-install/build' '/home/dieterle/ghc-eden/libraries/integer-gmp/dist-install/build' '/home/dieterle/ghc-eden/libraries/ghc-prim/dist-install/build' '/home/dieterle/ghc-eden/rts/dist/build'
libraries/containers_dist-install_DEP_EXTRA_LIBS = gmp m rt dl
libraries/containers_dist-install_DEP_LD_OPTS = 
libraries/containers_dist-install_BUILD_GHCI_LIB = YES

$(eval $(libraries/containers_PACKAGE_MAGIC))

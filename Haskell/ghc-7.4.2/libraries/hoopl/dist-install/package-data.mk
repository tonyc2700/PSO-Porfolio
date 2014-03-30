libraries/hoopl_dist-install_VERSION = 3.8.7.3
libraries/hoopl_dist-install_MODULES = Compiler.Hoopl Compiler.Hoopl.Wrappers Compiler.Hoopl.Passes.Dominator Compiler.Hoopl.Passes.DList Compiler.Hoopl.GHC Compiler.Hoopl.GraphUtil Compiler.Hoopl.Checkpoint Compiler.Hoopl.Collections Compiler.Hoopl.Combinators Compiler.Hoopl.Dataflow Compiler.Hoopl.Debug Compiler.Hoopl.Graph Compiler.Hoopl.Label Compiler.Hoopl.MkGraph Compiler.Hoopl.Fuel Compiler.Hoopl.Pointed Compiler.Hoopl.Shape Compiler.Hoopl.Show Compiler.Hoopl.Unique Compiler.Hoopl.Util Compiler.Hoopl.XUtil
libraries/hoopl_dist-install_HIDDEN_MODULES = Compiler.Hoopl.GraphUtil Compiler.Hoopl.Checkpoint Compiler.Hoopl.Collections Compiler.Hoopl.Combinators Compiler.Hoopl.Dataflow Compiler.Hoopl.Debug Compiler.Hoopl.Graph Compiler.Hoopl.Label Compiler.Hoopl.MkGraph Compiler.Hoopl.Fuel Compiler.Hoopl.Pointed Compiler.Hoopl.Shape Compiler.Hoopl.Show Compiler.Hoopl.Unique Compiler.Hoopl.Util Compiler.Hoopl.XUtil
libraries/hoopl_dist-install_SYNOPSIS =A library to support dataflow analysis and optimization
libraries/hoopl_dist-install_HS_SRC_DIRS = src
libraries/hoopl_dist-install_DEPS = base-4.5.1.0 containers-0.4.2.1
libraries/hoopl_dist-install_DEP_NAMES = base containers
libraries/hoopl_dist-install_INCLUDE_DIRS = 
libraries/hoopl_dist-install_INCLUDES = 
libraries/hoopl_dist-install_INSTALL_INCLUDES = 
libraries/hoopl_dist-install_EXTRA_LIBRARIES = 
libraries/hoopl_dist-install_EXTRA_LIBDIRS = 
libraries/hoopl_dist-install_C_SRCS  = 
libraries/hoopl_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard libraries/hoopl/cbits/*.cmm)))
libraries/hoopl_dist-install_DATA_FILES = 
libraries/hoopl_dist-install_HC_OPTS = -Wall -fno-warn-name-shadowing -XHaskell98 -XCPP
libraries/hoopl_dist-install_CC_OPTS = 
libraries/hoopl_dist-install_CPP_OPTS = 
libraries/hoopl_dist-install_LD_OPTS = 
libraries/hoopl_dist-install_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/containers/include' '/home/dieterle/ghc-eden/libraries/array/include' '/home/dieterle/ghc-eden/libraries/base/include' '/home/dieterle/ghc-eden/rts/dist/build' '/home/dieterle/ghc-eden/includes' '/home/dieterle/ghc-eden/includes/dist-ghcconstants/header' '/home/dieterle/ghc-eden/includes/dist-derivedconstants/header'
libraries/hoopl_dist-install_DEP_CC_OPTS = 
libraries/hoopl_dist-install_DEP_LIB_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/containers/dist-install/build' '/home/dieterle/ghc-eden/libraries/deepseq/dist-install/build' '/home/dieterle/ghc-eden/libraries/array/dist-install/build' '/home/dieterle/ghc-eden/libraries/base/dist-install/build' '/home/dieterle/ghc-eden/libraries/integer-gmp/dist-install/build' '/home/dieterle/ghc-eden/libraries/ghc-prim/dist-install/build' '/home/dieterle/ghc-eden/rts/dist/build'
libraries/hoopl_dist-install_DEP_EXTRA_LIBS = gmp m rt dl
libraries/hoopl_dist-install_DEP_LD_OPTS = 
libraries/hoopl_dist-install_BUILD_GHCI_LIB = YES

$(eval $(libraries/hoopl_PACKAGE_MAGIC))

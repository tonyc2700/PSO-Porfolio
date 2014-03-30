libraries/hoopl_dist-boot_VERSION = 3.8.7.3
libraries/hoopl_dist-boot_MODULES = Compiler.Hoopl Compiler.Hoopl.Wrappers Compiler.Hoopl.Passes.Dominator Compiler.Hoopl.Passes.DList Compiler.Hoopl.GHC Compiler.Hoopl.GraphUtil Compiler.Hoopl.Checkpoint Compiler.Hoopl.Collections Compiler.Hoopl.Combinators Compiler.Hoopl.Dataflow Compiler.Hoopl.Debug Compiler.Hoopl.Graph Compiler.Hoopl.Label Compiler.Hoopl.MkGraph Compiler.Hoopl.Fuel Compiler.Hoopl.Pointed Compiler.Hoopl.Shape Compiler.Hoopl.Show Compiler.Hoopl.Unique Compiler.Hoopl.Util Compiler.Hoopl.XUtil
libraries/hoopl_dist-boot_HIDDEN_MODULES = Compiler.Hoopl.GraphUtil Compiler.Hoopl.Checkpoint Compiler.Hoopl.Collections Compiler.Hoopl.Combinators Compiler.Hoopl.Dataflow Compiler.Hoopl.Debug Compiler.Hoopl.Graph Compiler.Hoopl.Label Compiler.Hoopl.MkGraph Compiler.Hoopl.Fuel Compiler.Hoopl.Pointed Compiler.Hoopl.Shape Compiler.Hoopl.Show Compiler.Hoopl.Unique Compiler.Hoopl.Util Compiler.Hoopl.XUtil
libraries/hoopl_dist-boot_SYNOPSIS =A library to support dataflow analysis and optimization
libraries/hoopl_dist-boot_HS_SRC_DIRS = src
libraries/hoopl_dist-boot_DEPS = base-4.3.1.0 containers-0.4.0.0
libraries/hoopl_dist-boot_DEP_NAMES = base containers
libraries/hoopl_dist-boot_INCLUDE_DIRS = 
libraries/hoopl_dist-boot_INCLUDES = 
libraries/hoopl_dist-boot_INSTALL_INCLUDES = 
libraries/hoopl_dist-boot_EXTRA_LIBRARIES = 
libraries/hoopl_dist-boot_EXTRA_LIBDIRS = 
libraries/hoopl_dist-boot_C_SRCS  = 
libraries/hoopl_dist-boot_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard libraries/hoopl/cbits/*.cmm)))
libraries/hoopl_dist-boot_DATA_FILES = 
libraries/hoopl_dist-boot_HC_OPTS = -Wall -fno-warn-name-shadowing -XHaskell98 -XCPP
libraries/hoopl_dist-boot_CC_OPTS = 
libraries/hoopl_dist-boot_CPP_OPTS = 
libraries/hoopl_dist-boot_LD_OPTS = 
libraries/hoopl_dist-boot_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/usr/lib/ghc-7.0.3/base-4.3.1.0/include' '/usr/lib/ghc-7.0.3/include'
libraries/hoopl_dist-boot_DEP_CC_OPTS = 
libraries/hoopl_dist-boot_DEP_LIB_DIRS_SINGLE_QUOTED = '/usr/lib/ghc-7.0.3/containers-0.4.0.0' '/usr/lib/ghc-7.0.3/array-0.3.0.2' '/usr/lib/ghc-7.0.3/base-4.3.1.0' '/usr/lib/ghc-7.0.3/integer-gmp-0.2.0.3' '/usr/lib/ghc-7.0.3/ghc-prim-0.2.0.0' '/usr/lib/ghc-7.0.3'
libraries/hoopl_dist-boot_DEP_EXTRA_LIBS = gmp ffi m rt dl
libraries/hoopl_dist-boot_DEP_LD_OPTS = 
libraries/hoopl_dist-boot_BUILD_GHCI_LIB = YES

$(eval $(libraries/hoopl_PACKAGE_MAGIC))

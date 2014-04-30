libraries/binary_dist-install_VERSION = 0.5.1.0
libraries/binary_dist-install_MODULES = Data.Binary Data.Binary.Put Data.Binary.Get Data.Binary.Builder Data.Binary.Builder.Internal Data.Binary.Builder.Base
libraries/binary_dist-install_HIDDEN_MODULES = Data.Binary.Builder.Base
libraries/binary_dist-install_SYNOPSIS =Binary serialisation for Haskell values using lazy ByteStrings
libraries/binary_dist-install_HS_SRC_DIRS = src
libraries/binary_dist-install_DEPS = array-0.4.0.0 base-4.5.1.0 bytestring-0.9.2.1 containers-0.4.2.1
libraries/binary_dist-install_DEP_NAMES = array base bytestring containers
libraries/binary_dist-install_INCLUDE_DIRS = 
libraries/binary_dist-install_INCLUDES = 
libraries/binary_dist-install_INSTALL_INCLUDES = 
libraries/binary_dist-install_EXTRA_LIBRARIES = 
libraries/binary_dist-install_EXTRA_LIBDIRS = 
libraries/binary_dist-install_C_SRCS  = 
libraries/binary_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard libraries/binary/cbits/*.cmm)))
libraries/binary_dist-install_DATA_FILES = 
libraries/binary_dist-install_HC_OPTS = -O2 -Wall -fliberate-case-threshold=1000 -XHaskell98 -XCPP -XFlexibleContexts
libraries/binary_dist-install_CC_OPTS = 
libraries/binary_dist-install_CPP_OPTS = -DAPPLICATIVE_IN_BASE
libraries/binary_dist-install_LD_OPTS = 
libraries/binary_dist-install_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/containers/include' '/home/dieterle/ghc-eden/libraries/bytestring/include' '/home/dieterle/ghc-eden/libraries/array/include' '/home/dieterle/ghc-eden/libraries/base/include' '/home/dieterle/ghc-eden/rts/dist/build' '/home/dieterle/ghc-eden/includes' '/home/dieterle/ghc-eden/includes/dist-ghcconstants/header' '/home/dieterle/ghc-eden/includes/dist-derivedconstants/header'
libraries/binary_dist-install_DEP_CC_OPTS = 
libraries/binary_dist-install_DEP_LIB_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/containers/dist-install/build' '/home/dieterle/ghc-eden/libraries/deepseq/dist-install/build' '/home/dieterle/ghc-eden/libraries/bytestring/dist-install/build' '/home/dieterle/ghc-eden/libraries/array/dist-install/build' '/home/dieterle/ghc-eden/libraries/base/dist-install/build' '/home/dieterle/ghc-eden/libraries/integer-gmp/dist-install/build' '/home/dieterle/ghc-eden/libraries/ghc-prim/dist-install/build' '/home/dieterle/ghc-eden/rts/dist/build'
libraries/binary_dist-install_DEP_EXTRA_LIBS = gmp m rt dl
libraries/binary_dist-install_DEP_LD_OPTS = 
libraries/binary_dist-install_BUILD_GHCI_LIB = YES

$(eval $(libraries/binary_PACKAGE_MAGIC))

libraries/array_dist-install_VERSION = 0.4.0.0
libraries/array_dist-install_MODULES = Data.Array.Base Data.Array.IArray Data.Array.IO Data.Array.IO.Safe Data.Array.IO.Internals Data.Array.MArray Data.Array.MArray.Safe Data.Array.ST Data.Array.ST.Safe Data.Array.Storable Data.Array.Storable.Safe Data.Array.Storable.Internals Data.Array.Unboxed Data.Array.Unsafe Data.Array
libraries/array_dist-install_HIDDEN_MODULES = 
libraries/array_dist-install_SYNOPSIS =Mutable and immutable arrays
libraries/array_dist-install_HS_SRC_DIRS = .
libraries/array_dist-install_DEPS = base-4.5.1.0
libraries/array_dist-install_DEP_NAMES = base
libraries/array_dist-install_INCLUDE_DIRS = include
libraries/array_dist-install_INCLUDES = 
libraries/array_dist-install_INSTALL_INCLUDES = 
libraries/array_dist-install_EXTRA_LIBRARIES = 
libraries/array_dist-install_EXTRA_LIBDIRS = 
libraries/array_dist-install_C_SRCS  = 
libraries/array_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard libraries/array/cbits/*.cmm)))
libraries/array_dist-install_DATA_FILES = 
libraries/array_dist-install_HC_OPTS = -XHaskell98 -XMultiParamTypeClasses -XFlexibleContexts -XFlexibleInstances -XTypeSynonymInstances -XDeriveDataTypeable -XStandaloneDeriving -XRank2Types -XMagicHash -XUnboxedTuples -XForeignFunctionInterface -XUnliftedFFITypes -XCPP
libraries/array_dist-install_CC_OPTS = 
libraries/array_dist-install_CPP_OPTS = 
libraries/array_dist-install_LD_OPTS = 
libraries/array_dist-install_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/base/include' '/home/dieterle/ghc-eden/rts/dist/build' '/home/dieterle/ghc-eden/includes' '/home/dieterle/ghc-eden/includes/dist-ghcconstants/header' '/home/dieterle/ghc-eden/includes/dist-derivedconstants/header'
libraries/array_dist-install_DEP_CC_OPTS = 
libraries/array_dist-install_DEP_LIB_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/base/dist-install/build' '/home/dieterle/ghc-eden/libraries/integer-gmp/dist-install/build' '/home/dieterle/ghc-eden/libraries/ghc-prim/dist-install/build' '/home/dieterle/ghc-eden/rts/dist/build'
libraries/array_dist-install_DEP_EXTRA_LIBS = gmp m rt dl
libraries/array_dist-install_DEP_LD_OPTS = 
libraries/array_dist-install_BUILD_GHCI_LIB = YES

$(eval $(libraries/array_PACKAGE_MAGIC))

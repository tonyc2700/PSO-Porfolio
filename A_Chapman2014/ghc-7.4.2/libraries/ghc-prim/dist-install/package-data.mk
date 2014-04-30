libraries/ghc-prim_dist-install_VERSION = 0.2.0.0
libraries/ghc-prim_dist-install_MODULES = GHC.Prim GHC.Classes GHC.CString GHC.Debug GHC.Generics GHC.Magic GHC.PrimopWrappers GHC.IntWord64 GHC.Tuple GHC.Types
libraries/ghc-prim_dist-install_HIDDEN_MODULES = 
libraries/ghc-prim_dist-install_SYNOPSIS =GHC primitives
libraries/ghc-prim_dist-install_HS_SRC_DIRS = .
libraries/ghc-prim_dist-install_DEPS = rts-1.0
libraries/ghc-prim_dist-install_DEP_NAMES = rts
libraries/ghc-prim_dist-install_INCLUDE_DIRS = 
libraries/ghc-prim_dist-install_INCLUDES = 
libraries/ghc-prim_dist-install_INSTALL_INCLUDES = 
libraries/ghc-prim_dist-install_EXTRA_LIBRARIES = 
libraries/ghc-prim_dist-install_EXTRA_LIBDIRS = 
libraries/ghc-prim_dist-install_C_SRCS  = cbits/debug.c cbits/longlong.c cbits/popcnt.c
libraries/ghc-prim_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard libraries/ghc-prim/cbits/*.cmm)))
libraries/ghc-prim_dist-install_DATA_FILES = 
libraries/ghc-prim_dist-install_HC_OPTS = -package-name ghc-prim -XHaskell98 -XCPP -XMagicHash -XForeignFunctionInterface -XUnliftedFFITypes -XUnboxedTuples -XEmptyDataDecls -XNoImplicitPrelude
libraries/ghc-prim_dist-install_CC_OPTS = 
libraries/ghc-prim_dist-install_CPP_OPTS = 
libraries/ghc-prim_dist-install_LD_OPTS = 
libraries/ghc-prim_dist-install_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/rts/dist/build' '/home/dieterle/ghc-eden/includes' '/home/dieterle/ghc-eden/includes/dist-ghcconstants/header' '/home/dieterle/ghc-eden/includes/dist-derivedconstants/header'
libraries/ghc-prim_dist-install_DEP_CC_OPTS = 
libraries/ghc-prim_dist-install_DEP_LIB_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/rts/dist/build'
libraries/ghc-prim_dist-install_DEP_EXTRA_LIBS = m rt dl
libraries/ghc-prim_dist-install_DEP_LD_OPTS = 
libraries/ghc-prim_dist-install_BUILD_GHCI_LIB = YES

$(eval $(libraries/ghc-prim_PACKAGE_MAGIC))

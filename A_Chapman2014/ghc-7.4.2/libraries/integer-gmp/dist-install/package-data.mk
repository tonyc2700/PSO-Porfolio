libraries/integer-gmp_dist-install_VERSION = 0.4.0.0
libraries/integer-gmp_dist-install_MODULES = GHC.Integer GHC.Integer.GMP.Internals GHC.Integer.GMP.Prim GHC.Integer.Logarithms GHC.Integer.Logarithms.Internals GHC.Integer.Type
libraries/integer-gmp_dist-install_HIDDEN_MODULES = GHC.Integer.Type
libraries/integer-gmp_dist-install_SYNOPSIS =Integer library based on GMP
libraries/integer-gmp_dist-install_HS_SRC_DIRS = .
libraries/integer-gmp_dist-install_DEPS = ghc-prim-0.2.0.0
libraries/integer-gmp_dist-install_DEP_NAMES = ghc-prim
libraries/integer-gmp_dist-install_INCLUDE_DIRS = 
libraries/integer-gmp_dist-install_INCLUDES = 
libraries/integer-gmp_dist-install_INSTALL_INCLUDES = 
libraries/integer-gmp_dist-install_EXTRA_LIBRARIES = gmp
libraries/integer-gmp_dist-install_EXTRA_LIBDIRS = 
libraries/integer-gmp_dist-install_C_SRCS  = cbits/cbits.c
libraries/integer-gmp_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard libraries/integer-gmp/cbits/*.cmm)))
libraries/integer-gmp_dist-install_DATA_FILES = 
libraries/integer-gmp_dist-install_HC_OPTS = -package-name integer-gmp -XHaskell98 -XCPP -XMagicHash -XUnboxedTuples -XNoImplicitPrelude -XForeignFunctionInterface -XUnliftedFFITypes
libraries/integer-gmp_dist-install_CC_OPTS = 
libraries/integer-gmp_dist-install_CPP_OPTS = 
libraries/integer-gmp_dist-install_LD_OPTS = 
libraries/integer-gmp_dist-install_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/rts/dist/build' '/home/dieterle/ghc-eden/includes' '/home/dieterle/ghc-eden/includes/dist-ghcconstants/header' '/home/dieterle/ghc-eden/includes/dist-derivedconstants/header'
libraries/integer-gmp_dist-install_DEP_CC_OPTS = 
libraries/integer-gmp_dist-install_DEP_LIB_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/ghc-prim/dist-install/build' '/home/dieterle/ghc-eden/rts/dist/build'
libraries/integer-gmp_dist-install_DEP_EXTRA_LIBS = m rt dl
libraries/integer-gmp_dist-install_DEP_LD_OPTS = 
libraries/integer-gmp_dist-install_BUILD_GHCI_LIB = YES

$(eval $(libraries/integer-gmp_PACKAGE_MAGIC))

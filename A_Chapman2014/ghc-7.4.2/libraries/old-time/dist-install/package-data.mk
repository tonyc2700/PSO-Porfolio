libraries/old-time_dist-install_VERSION = 1.1.0.0
libraries/old-time_dist-install_MODULES = System.Time
libraries/old-time_dist-install_HIDDEN_MODULES = 
libraries/old-time_dist-install_SYNOPSIS =Time library
libraries/old-time_dist-install_HS_SRC_DIRS = .
libraries/old-time_dist-install_DEPS = base-4.5.1.0 old-locale-1.0.0.4
libraries/old-time_dist-install_DEP_NAMES = base old-locale
libraries/old-time_dist-install_INCLUDE_DIRS = include
libraries/old-time_dist-install_INCLUDES = HsTime.h
libraries/old-time_dist-install_INSTALL_INCLUDES = HsTime.h HsTimeConfig.h
libraries/old-time_dist-install_EXTRA_LIBRARIES = 
libraries/old-time_dist-install_EXTRA_LIBDIRS = 
libraries/old-time_dist-install_C_SRCS  = cbits/timeUtils.c
libraries/old-time_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard libraries/old-time/cbits/*.cmm)))
libraries/old-time_dist-install_DATA_FILES = 
libraries/old-time_dist-install_HC_OPTS = -XHaskell98 -XCPP -XForeignFunctionInterface
libraries/old-time_dist-install_CC_OPTS = 
libraries/old-time_dist-install_CPP_OPTS = 
libraries/old-time_dist-install_LD_OPTS = 
libraries/old-time_dist-install_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/base/include' '/home/dieterle/ghc-eden/rts/dist/build' '/home/dieterle/ghc-eden/includes' '/home/dieterle/ghc-eden/includes/dist-ghcconstants/header' '/home/dieterle/ghc-eden/includes/dist-derivedconstants/header'
libraries/old-time_dist-install_DEP_CC_OPTS = 
libraries/old-time_dist-install_DEP_LIB_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/old-locale/dist-install/build' '/home/dieterle/ghc-eden/libraries/base/dist-install/build' '/home/dieterle/ghc-eden/libraries/integer-gmp/dist-install/build' '/home/dieterle/ghc-eden/libraries/ghc-prim/dist-install/build' '/home/dieterle/ghc-eden/rts/dist/build'
libraries/old-time_dist-install_DEP_EXTRA_LIBS = gmp m rt dl
libraries/old-time_dist-install_DEP_LD_OPTS = 
libraries/old-time_dist-install_BUILD_GHCI_LIB = YES

$(eval $(libraries/old-time_PACKAGE_MAGIC))

libraries/directory_dist-install_VERSION = 1.1.0.2
libraries/directory_dist-install_MODULES = System.Directory
libraries/directory_dist-install_HIDDEN_MODULES = 
libraries/directory_dist-install_SYNOPSIS =library for directory handling
libraries/directory_dist-install_HS_SRC_DIRS = .
libraries/directory_dist-install_DEPS = base-4.5.1.0 filepath-1.3.0.0 old-time-1.1.0.0 unix-2.5.1.1
libraries/directory_dist-install_DEP_NAMES = base filepath old-time unix
libraries/directory_dist-install_INCLUDE_DIRS = include
libraries/directory_dist-install_INCLUDES = HsDirectory.h
libraries/directory_dist-install_INSTALL_INCLUDES = HsDirectory.h HsDirectoryConfig.h
libraries/directory_dist-install_EXTRA_LIBRARIES = 
libraries/directory_dist-install_EXTRA_LIBDIRS = 
libraries/directory_dist-install_C_SRCS  = cbits/directory.c
libraries/directory_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard libraries/directory/cbits/*.cmm)))
libraries/directory_dist-install_DATA_FILES = 
libraries/directory_dist-install_HC_OPTS = -XHaskell98 -XNondecreasingIndentation -XCPP -XForeignFunctionInterface
libraries/directory_dist-install_CC_OPTS = 
libraries/directory_dist-install_CPP_OPTS = 
libraries/directory_dist-install_LD_OPTS = 
libraries/directory_dist-install_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/unix/include' '/home/dieterle/ghc-eden/libraries/old-time/include' '/home/dieterle/ghc-eden/libraries/bytestring/include' '/home/dieterle/ghc-eden/libraries/base/include' '/home/dieterle/ghc-eden/rts/dist/build' '/home/dieterle/ghc-eden/includes' '/home/dieterle/ghc-eden/includes/dist-ghcconstants/header' '/home/dieterle/ghc-eden/includes/dist-derivedconstants/header'
libraries/directory_dist-install_DEP_CC_OPTS = 
libraries/directory_dist-install_DEP_LIB_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/unix/dist-install/build' '/home/dieterle/ghc-eden/libraries/old-time/dist-install/build' '/home/dieterle/ghc-eden/libraries/old-locale/dist-install/build' '/home/dieterle/ghc-eden/libraries/filepath/dist-install/build' '/home/dieterle/ghc-eden/libraries/bytestring/dist-install/build' '/home/dieterle/ghc-eden/libraries/base/dist-install/build' '/home/dieterle/ghc-eden/libraries/integer-gmp/dist-install/build' '/home/dieterle/ghc-eden/libraries/ghc-prim/dist-install/build' '/home/dieterle/ghc-eden/rts/dist/build'
libraries/directory_dist-install_DEP_EXTRA_LIBS = rt util dl pthread gmp m rt dl
libraries/directory_dist-install_DEP_LD_OPTS = 
libraries/directory_dist-install_BUILD_GHCI_LIB = YES

$(eval $(libraries/directory_PACKAGE_MAGIC))

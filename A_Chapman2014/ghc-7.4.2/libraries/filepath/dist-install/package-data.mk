libraries/filepath_dist-install_VERSION = 1.3.0.0
libraries/filepath_dist-install_MODULES = System.FilePath System.FilePath.Posix System.FilePath.Windows
libraries/filepath_dist-install_HIDDEN_MODULES = 
libraries/filepath_dist-install_SYNOPSIS =Library for manipulating FilePaths in a cross platform way.
libraries/filepath_dist-install_HS_SRC_DIRS = .
libraries/filepath_dist-install_DEPS = base-4.5.1.0
libraries/filepath_dist-install_DEP_NAMES = base
libraries/filepath_dist-install_INCLUDE_DIRS = 
libraries/filepath_dist-install_INCLUDES = 
libraries/filepath_dist-install_INSTALL_INCLUDES = 
libraries/filepath_dist-install_EXTRA_LIBRARIES = 
libraries/filepath_dist-install_EXTRA_LIBDIRS = 
libraries/filepath_dist-install_C_SRCS  = 
libraries/filepath_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard libraries/filepath/cbits/*.cmm)))
libraries/filepath_dist-install_DATA_FILES = 
libraries/filepath_dist-install_HC_OPTS = -XHaskell98 -XCPP
libraries/filepath_dist-install_CC_OPTS = 
libraries/filepath_dist-install_CPP_OPTS = 
libraries/filepath_dist-install_LD_OPTS = 
libraries/filepath_dist-install_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/base/include' '/home/dieterle/ghc-eden/rts/dist/build' '/home/dieterle/ghc-eden/includes' '/home/dieterle/ghc-eden/includes/dist-ghcconstants/header' '/home/dieterle/ghc-eden/includes/dist-derivedconstants/header'
libraries/filepath_dist-install_DEP_CC_OPTS = 
libraries/filepath_dist-install_DEP_LIB_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/base/dist-install/build' '/home/dieterle/ghc-eden/libraries/integer-gmp/dist-install/build' '/home/dieterle/ghc-eden/libraries/ghc-prim/dist-install/build' '/home/dieterle/ghc-eden/rts/dist/build'
libraries/filepath_dist-install_DEP_EXTRA_LIBS = gmp m rt dl
libraries/filepath_dist-install_DEP_LD_OPTS = 
libraries/filepath_dist-install_BUILD_GHCI_LIB = YES

$(eval $(libraries/filepath_PACKAGE_MAGIC))

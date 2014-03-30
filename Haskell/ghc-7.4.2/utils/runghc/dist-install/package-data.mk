utils/runghc_dist-install_VERSION = 7.4.2
utils/runghc_dist-install_MODULES = Main
utils/runghc_dist-install_HIDDEN_MODULES = 
utils/runghc_dist-install_SYNOPSIS =XXX
utils/runghc_dist-install_HS_SRC_DIRS = .
utils/runghc_dist-install_DEPS = base-4.5.1.0 directory-1.1.0.2 filepath-1.3.0.0 process-1.1.0.1
utils/runghc_dist-install_DEP_NAMES = base directory filepath process
utils/runghc_dist-install_INCLUDE_DIRS = 
utils/runghc_dist-install_INCLUDES = 
utils/runghc_dist-install_INSTALL_INCLUDES = 
utils/runghc_dist-install_EXTRA_LIBRARIES = 
utils/runghc_dist-install_EXTRA_LIBDIRS = 
utils/runghc_dist-install_C_SRCS  = 
utils/runghc_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard utils/runghc/cbits/*.cmm)))
utils/runghc_dist-install_DATA_FILES = 
utils/runghc_dist-install_HC_OPTS = -XHaskell98
utils/runghc_dist-install_CC_OPTS = 
utils/runghc_dist-install_CPP_OPTS = 
utils/runghc_dist-install_LD_OPTS = 
utils/runghc_dist-install_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/process/include' '/home/dieterle/ghc-eden/libraries/directory/include' '/home/dieterle/ghc-eden/libraries/unix/include' '/home/dieterle/ghc-eden/libraries/old-time/include' '/home/dieterle/ghc-eden/libraries/bytestring/include' '/home/dieterle/ghc-eden/libraries/base/include' '/home/dieterle/ghc-eden/rts/dist/build' '/home/dieterle/ghc-eden/includes' '/home/dieterle/ghc-eden/includes/dist-ghcconstants/header' '/home/dieterle/ghc-eden/includes/dist-derivedconstants/header'
utils/runghc_dist-install_DEP_CC_OPTS = 
utils/runghc_dist-install_DEP_LIB_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/process/dist-install/build' '/home/dieterle/ghc-eden/libraries/directory/dist-install/build' '/home/dieterle/ghc-eden/libraries/unix/dist-install/build' '/home/dieterle/ghc-eden/libraries/old-time/dist-install/build' '/home/dieterle/ghc-eden/libraries/old-locale/dist-install/build' '/home/dieterle/ghc-eden/libraries/filepath/dist-install/build' '/home/dieterle/ghc-eden/libraries/bytestring/dist-install/build' '/home/dieterle/ghc-eden/libraries/base/dist-install/build' '/home/dieterle/ghc-eden/libraries/integer-gmp/dist-install/build' '/home/dieterle/ghc-eden/libraries/ghc-prim/dist-install/build' '/home/dieterle/ghc-eden/rts/dist/build'
utils/runghc_dist-install_DEP_EXTRA_LIBS = rt util dl pthread gmp m rt dl
utils/runghc_dist-install_DEP_LD_OPTS = 
utils/runghc_dist-install_BUILD_GHCI_LIB = YES

$(eval $(utils/runghc_PACKAGE_MAGIC))

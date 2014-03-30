libraries/stm_dist-install_VERSION = 2.3
libraries/stm_dist-install_MODULES = Control.Concurrent.STM Control.Concurrent.STM.TArray Control.Concurrent.STM.TVar Control.Concurrent.STM.TChan Control.Concurrent.STM.TMVar Control.Monad.STM Control.Sequential.STM
libraries/stm_dist-install_HIDDEN_MODULES = Control.Sequential.STM
libraries/stm_dist-install_SYNOPSIS =Software Transactional Memory
libraries/stm_dist-install_HS_SRC_DIRS = .
libraries/stm_dist-install_DEPS = array-0.4.0.0 base-4.5.1.0
libraries/stm_dist-install_DEP_NAMES = array base
libraries/stm_dist-install_INCLUDE_DIRS = 
libraries/stm_dist-install_INCLUDES = 
libraries/stm_dist-install_INSTALL_INCLUDES = 
libraries/stm_dist-install_EXTRA_LIBRARIES = 
libraries/stm_dist-install_EXTRA_LIBDIRS = 
libraries/stm_dist-install_C_SRCS  = 
libraries/stm_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard libraries/stm/cbits/*.cmm)))
libraries/stm_dist-install_DATA_FILES = 
libraries/stm_dist-install_HC_OPTS = -XHaskell98
libraries/stm_dist-install_CC_OPTS = 
libraries/stm_dist-install_CPP_OPTS = -DBASE4
libraries/stm_dist-install_LD_OPTS = 
libraries/stm_dist-install_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/array/include' '/home/dieterle/ghc-eden/libraries/base/include' '/home/dieterle/ghc-eden/rts/dist/build' '/home/dieterle/ghc-eden/includes' '/home/dieterle/ghc-eden/includes/dist-ghcconstants/header' '/home/dieterle/ghc-eden/includes/dist-derivedconstants/header'
libraries/stm_dist-install_DEP_CC_OPTS = 
libraries/stm_dist-install_DEP_LIB_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/array/dist-install/build' '/home/dieterle/ghc-eden/libraries/base/dist-install/build' '/home/dieterle/ghc-eden/libraries/integer-gmp/dist-install/build' '/home/dieterle/ghc-eden/libraries/ghc-prim/dist-install/build' '/home/dieterle/ghc-eden/rts/dist/build'
libraries/stm_dist-install_DEP_EXTRA_LIBS = gmp m rt dl
libraries/stm_dist-install_DEP_LD_OPTS = 
libraries/stm_dist-install_BUILD_GHCI_LIB = YES

$(eval $(libraries/stm_PACKAGE_MAGIC))

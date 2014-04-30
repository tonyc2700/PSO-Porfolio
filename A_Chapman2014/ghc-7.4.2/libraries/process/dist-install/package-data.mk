libraries/process_dist-install_VERSION = 1.1.0.1
libraries/process_dist-install_MODULES = System.Process.Internals System.Process System.Cmd
libraries/process_dist-install_HIDDEN_MODULES = 
libraries/process_dist-install_SYNOPSIS =Process libraries
libraries/process_dist-install_HS_SRC_DIRS = .
libraries/process_dist-install_DEPS = base-4.5.1.0 directory-1.1.0.2 filepath-1.3.0.0 unix-2.5.1.1
libraries/process_dist-install_DEP_NAMES = base directory filepath unix
libraries/process_dist-install_INCLUDE_DIRS = include
libraries/process_dist-install_INCLUDES = runProcess.h
libraries/process_dist-install_INSTALL_INCLUDES = runProcess.h processFlags.h HsProcessConfig.h
libraries/process_dist-install_EXTRA_LIBRARIES = 
libraries/process_dist-install_EXTRA_LIBDIRS = 
libraries/process_dist-install_C_SRCS  = cbits/runProcess.c
libraries/process_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard libraries/process/cbits/*.cmm)))
libraries/process_dist-install_DATA_FILES = 
libraries/process_dist-install_HC_OPTS = -XHaskell98 -XCPP
libraries/process_dist-install_CC_OPTS = 
libraries/process_dist-install_CPP_OPTS = -Dbase4
libraries/process_dist-install_LD_OPTS = 
libraries/process_dist-install_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/directory/include' '/home/dieterle/ghc-eden/libraries/unix/include' '/home/dieterle/ghc-eden/libraries/old-time/include' '/home/dieterle/ghc-eden/libraries/bytestring/include' '/home/dieterle/ghc-eden/libraries/base/include' '/home/dieterle/ghc-eden/rts/dist/build' '/home/dieterle/ghc-eden/includes' '/home/dieterle/ghc-eden/includes/dist-ghcconstants/header' '/home/dieterle/ghc-eden/includes/dist-derivedconstants/header'
libraries/process_dist-install_DEP_CC_OPTS = 
libraries/process_dist-install_DEP_LIB_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/directory/dist-install/build' '/home/dieterle/ghc-eden/libraries/unix/dist-install/build' '/home/dieterle/ghc-eden/libraries/old-time/dist-install/build' '/home/dieterle/ghc-eden/libraries/old-locale/dist-install/build' '/home/dieterle/ghc-eden/libraries/filepath/dist-install/build' '/home/dieterle/ghc-eden/libraries/bytestring/dist-install/build' '/home/dieterle/ghc-eden/libraries/base/dist-install/build' '/home/dieterle/ghc-eden/libraries/integer-gmp/dist-install/build' '/home/dieterle/ghc-eden/libraries/ghc-prim/dist-install/build' '/home/dieterle/ghc-eden/rts/dist/build'
libraries/process_dist-install_DEP_EXTRA_LIBS = rt util dl pthread gmp m rt dl
libraries/process_dist-install_DEP_LD_OPTS = 
libraries/process_dist-install_BUILD_GHCI_LIB = YES

$(eval $(libraries/process_PACKAGE_MAGIC))

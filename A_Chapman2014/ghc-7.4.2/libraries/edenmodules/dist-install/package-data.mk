libraries/edenmodules_dist-install_VERSION = 1.1.0.1
libraries/edenmodules_dist-install_MODULES = Control.Parallel.Eden Control.Parallel.Eden.ParPrim Control.Parallel.Eden.ParPrimConcHs Control.Parallel.Eden.EdenConcHs Control.Parallel.Eden.Edi
libraries/edenmodules_dist-install_HIDDEN_MODULES = 
libraries/edenmodules_dist-install_SYNOPSIS =Semi-explicit parallel programming library
libraries/edenmodules_dist-install_HS_SRC_DIRS = .
libraries/edenmodules_dist-install_DEPS = base-4.5.1.0 containers-0.4.2.1 deepseq-1.3.0.0 parallel-3.2.0.3
libraries/edenmodules_dist-install_DEP_NAMES = base containers deepseq parallel
libraries/edenmodules_dist-install_INCLUDE_DIRS = 
libraries/edenmodules_dist-install_INCLUDES = 
libraries/edenmodules_dist-install_INSTALL_INCLUDES = 
libraries/edenmodules_dist-install_EXTRA_LIBRARIES = 
libraries/edenmodules_dist-install_EXTRA_LIBDIRS = 
libraries/edenmodules_dist-install_C_SRCS  = 
libraries/edenmodules_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard libraries/edenmodules/cbits/*.cmm)))
libraries/edenmodules_dist-install_DATA_FILES = 
libraries/edenmodules_dist-install_HC_OPTS = -XHaskell98 -XCPP
libraries/edenmodules_dist-install_CC_OPTS = 
libraries/edenmodules_dist-install_CPP_OPTS = 
libraries/edenmodules_dist-install_LD_OPTS = 
libraries/edenmodules_dist-install_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/containers/include' '/home/dieterle/ghc-eden/libraries/array/include' '/home/dieterle/ghc-eden/libraries/base/include' '/home/dieterle/ghc-eden/rts/dist/build' '/home/dieterle/ghc-eden/includes' '/home/dieterle/ghc-eden/includes/dist-ghcconstants/header' '/home/dieterle/ghc-eden/includes/dist-derivedconstants/header'
libraries/edenmodules_dist-install_DEP_CC_OPTS = 
libraries/edenmodules_dist-install_DEP_LIB_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/parallel/dist-install/build' '/home/dieterle/ghc-eden/libraries/containers/dist-install/build' '/home/dieterle/ghc-eden/libraries/deepseq/dist-install/build' '/home/dieterle/ghc-eden/libraries/array/dist-install/build' '/home/dieterle/ghc-eden/libraries/base/dist-install/build' '/home/dieterle/ghc-eden/libraries/integer-gmp/dist-install/build' '/home/dieterle/ghc-eden/libraries/ghc-prim/dist-install/build' '/home/dieterle/ghc-eden/rts/dist/build'
libraries/edenmodules_dist-install_DEP_EXTRA_LIBS = gmp m rt dl
libraries/edenmodules_dist-install_DEP_LD_OPTS = 
libraries/edenmodules_dist-install_BUILD_GHCI_LIB = YES

$(eval $(libraries/edenmodules_PACKAGE_MAGIC))

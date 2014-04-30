libraries/haskell98_dist-install_VERSION = 2.0.0.1
libraries/haskell98_dist-install_MODULES = Prelude Array CPUTime Char Complex Directory IO Ix List Locale Maybe Monad Numeric Random Ratio System Time Bits CError CForeign CString CTypes ForeignPtr Int MarshalAlloc MarshalArray MarshalError MarshalUtils Ptr StablePtr Storable Word
libraries/haskell98_dist-install_HIDDEN_MODULES = 
libraries/haskell98_dist-install_SYNOPSIS =Compatibility with Haskell 98
libraries/haskell98_dist-install_HS_SRC_DIRS = .
libraries/haskell98_dist-install_DEPS = array-0.4.0.0 base-4.5.1.0 directory-1.1.0.2 old-locale-1.0.0.4 old-time-1.1.0.0 process-1.1.0.1 time-1.4
libraries/haskell98_dist-install_DEP_NAMES = array base directory old-locale old-time process time
libraries/haskell98_dist-install_INCLUDE_DIRS = 
libraries/haskell98_dist-install_INCLUDES = 
libraries/haskell98_dist-install_INSTALL_INCLUDES = 
libraries/haskell98_dist-install_EXTRA_LIBRARIES = 
libraries/haskell98_dist-install_EXTRA_LIBDIRS = 
libraries/haskell98_dist-install_C_SRCS  = 
libraries/haskell98_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard libraries/haskell98/cbits/*.cmm)))
libraries/haskell98_dist-install_DATA_FILES = 
libraries/haskell98_dist-install_HC_OPTS = -XHaskell98 -XCPP
libraries/haskell98_dist-install_CC_OPTS = 
libraries/haskell98_dist-install_CPP_OPTS = 
libraries/haskell98_dist-install_LD_OPTS = 
libraries/haskell98_dist-install_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/time/include' '/home/dieterle/ghc-eden/libraries/process/include' '/home/dieterle/ghc-eden/libraries/directory/include' '/home/dieterle/ghc-eden/libraries/unix/include' '/home/dieterle/ghc-eden/libraries/old-time/include' '/home/dieterle/ghc-eden/libraries/bytestring/include' '/home/dieterle/ghc-eden/libraries/array/include' '/home/dieterle/ghc-eden/libraries/base/include' '/home/dieterle/ghc-eden/rts/dist/build' '/home/dieterle/ghc-eden/includes' '/home/dieterle/ghc-eden/includes/dist-ghcconstants/header' '/home/dieterle/ghc-eden/includes/dist-derivedconstants/header'
libraries/haskell98_dist-install_DEP_CC_OPTS = 
libraries/haskell98_dist-install_DEP_LIB_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/time/dist-install/build' '/home/dieterle/ghc-eden/libraries/process/dist-install/build' '/home/dieterle/ghc-eden/libraries/directory/dist-install/build' '/home/dieterle/ghc-eden/libraries/unix/dist-install/build' '/home/dieterle/ghc-eden/libraries/old-time/dist-install/build' '/home/dieterle/ghc-eden/libraries/old-locale/dist-install/build' '/home/dieterle/ghc-eden/libraries/filepath/dist-install/build' '/home/dieterle/ghc-eden/libraries/deepseq/dist-install/build' '/home/dieterle/ghc-eden/libraries/bytestring/dist-install/build' '/home/dieterle/ghc-eden/libraries/array/dist-install/build' '/home/dieterle/ghc-eden/libraries/base/dist-install/build' '/home/dieterle/ghc-eden/libraries/integer-gmp/dist-install/build' '/home/dieterle/ghc-eden/libraries/ghc-prim/dist-install/build' '/home/dieterle/ghc-eden/rts/dist/build'
libraries/haskell98_dist-install_DEP_EXTRA_LIBS = rt util dl pthread gmp m rt dl
libraries/haskell98_dist-install_DEP_LD_OPTS = 
libraries/haskell98_dist-install_BUILD_GHCI_LIB = YES

$(eval $(libraries/haskell98_PACKAGE_MAGIC))

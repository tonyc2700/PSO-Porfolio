libraries/mtl_dist-install_VERSION = 1.1.1.1
libraries/mtl_dist-install_MODULES = Control.Monad.Cont Control.Monad.Cont.Class Control.Monad.Error Control.Monad.Error.Class Control.Monad.Identity Control.Monad.List Control.Monad.RWS Control.Monad.RWS.Class Control.Monad.RWS.Lazy Control.Monad.RWS.Strict Control.Monad.Reader Control.Monad.Reader.Class Control.Monad.State Control.Monad.State.Class Control.Monad.State.Lazy Control.Monad.State.Strict Control.Monad.Trans Control.Monad.Writer Control.Monad.Writer.Class Control.Monad.Writer.Lazy Control.Monad.Writer.Strict
libraries/mtl_dist-install_HIDDEN_MODULES = 
libraries/mtl_dist-install_SYNOPSIS =Monad transformer library
libraries/mtl_dist-install_HS_SRC_DIRS = .
libraries/mtl_dist-install_DEPS = base-4.5.1.0
libraries/mtl_dist-install_DEP_NAMES = base
libraries/mtl_dist-install_INCLUDE_DIRS = 
libraries/mtl_dist-install_INCLUDES = 
libraries/mtl_dist-install_INSTALL_INCLUDES = 
libraries/mtl_dist-install_EXTRA_LIBRARIES = 
libraries/mtl_dist-install_EXTRA_LIBDIRS = 
libraries/mtl_dist-install_C_SRCS  = 
libraries/mtl_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard libraries/mtl/cbits/*.cmm)))
libraries/mtl_dist-install_DATA_FILES = 
libraries/mtl_dist-install_HC_OPTS = -Wall -XHaskell98 -XSafe -XMultiParamTypeClasses -XFunctionalDependencies -XFlexibleInstances -XTypeSynonymInstances
libraries/mtl_dist-install_CC_OPTS = 
libraries/mtl_dist-install_CPP_OPTS = 
libraries/mtl_dist-install_LD_OPTS = 
libraries/mtl_dist-install_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/base/include' '/home/dieterle/ghc-eden/rts/dist/build' '/home/dieterle/ghc-eden/includes' '/home/dieterle/ghc-eden/includes/dist-ghcconstants/header' '/home/dieterle/ghc-eden/includes/dist-derivedconstants/header'
libraries/mtl_dist-install_DEP_CC_OPTS = 
libraries/mtl_dist-install_DEP_LIB_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/base/dist-install/build' '/home/dieterle/ghc-eden/libraries/integer-gmp/dist-install/build' '/home/dieterle/ghc-eden/libraries/ghc-prim/dist-install/build' '/home/dieterle/ghc-eden/rts/dist/build'
libraries/mtl_dist-install_DEP_EXTRA_LIBS = gmp m rt dl
libraries/mtl_dist-install_DEP_LD_OPTS = 
libraries/mtl_dist-install_BUILD_GHCI_LIB = YES

$(eval $(libraries/mtl_PACKAGE_MAGIC))

libraries/template-haskell_dist-install_VERSION = 2.7.0.0
libraries/template-haskell_dist-install_MODULES = Language.Haskell.TH.Syntax.Internals Language.Haskell.TH.Syntax Language.Haskell.TH.PprLib Language.Haskell.TH.Ppr Language.Haskell.TH.Lib Language.Haskell.TH.Quote Language.Haskell.TH
libraries/template-haskell_dist-install_HIDDEN_MODULES = 
libraries/template-haskell_dist-install_SYNOPSIS =
libraries/template-haskell_dist-install_HS_SRC_DIRS = .
libraries/template-haskell_dist-install_DEPS = base-4.5.1.0 containers-0.4.2.1 pretty-1.1.1.0
libraries/template-haskell_dist-install_DEP_NAMES = base containers pretty
libraries/template-haskell_dist-install_INCLUDE_DIRS = 
libraries/template-haskell_dist-install_INCLUDES = 
libraries/template-haskell_dist-install_INSTALL_INCLUDES = 
libraries/template-haskell_dist-install_EXTRA_LIBRARIES = 
libraries/template-haskell_dist-install_EXTRA_LIBDIRS = 
libraries/template-haskell_dist-install_C_SRCS  = 
libraries/template-haskell_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard libraries/template-haskell/cbits/*.cmm)))
libraries/template-haskell_dist-install_DATA_FILES = 
libraries/template-haskell_dist-install_HC_OPTS = -package-name template-haskell -XHaskell98 -XMagicHash -XPatternGuards -XPolymorphicComponents -XDeriveDataTypeable
libraries/template-haskell_dist-install_CC_OPTS = 
libraries/template-haskell_dist-install_CPP_OPTS = 
libraries/template-haskell_dist-install_LD_OPTS = 
libraries/template-haskell_dist-install_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/containers/include' '/home/dieterle/ghc-eden/libraries/array/include' '/home/dieterle/ghc-eden/libraries/base/include' '/home/dieterle/ghc-eden/rts/dist/build' '/home/dieterle/ghc-eden/includes' '/home/dieterle/ghc-eden/includes/dist-ghcconstants/header' '/home/dieterle/ghc-eden/includes/dist-derivedconstants/header'
libraries/template-haskell_dist-install_DEP_CC_OPTS = 
libraries/template-haskell_dist-install_DEP_LIB_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/pretty/dist-install/build' '/home/dieterle/ghc-eden/libraries/containers/dist-install/build' '/home/dieterle/ghc-eden/libraries/deepseq/dist-install/build' '/home/dieterle/ghc-eden/libraries/array/dist-install/build' '/home/dieterle/ghc-eden/libraries/base/dist-install/build' '/home/dieterle/ghc-eden/libraries/integer-gmp/dist-install/build' '/home/dieterle/ghc-eden/libraries/ghc-prim/dist-install/build' '/home/dieterle/ghc-eden/rts/dist/build'
libraries/template-haskell_dist-install_DEP_EXTRA_LIBS = gmp m rt dl
libraries/template-haskell_dist-install_DEP_LD_OPTS = 
libraries/template-haskell_dist-install_BUILD_GHCI_LIB = YES

$(eval $(libraries/template-haskell_PACKAGE_MAGIC))

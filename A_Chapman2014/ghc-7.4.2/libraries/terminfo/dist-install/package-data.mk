libraries/terminfo_dist-install_VERSION = 0.3.2.3
libraries/terminfo_dist-install_MODULES = System.Console.Terminfo System.Console.Terminfo.Base System.Console.Terminfo.Cursor System.Console.Terminfo.Color System.Console.Terminfo.Edit System.Console.Terminfo.Effects System.Console.Terminfo.Keys
libraries/terminfo_dist-install_HIDDEN_MODULES = 
libraries/terminfo_dist-install_SYNOPSIS =Haskell bindings to the terminfo library.
libraries/terminfo_dist-install_HS_SRC_DIRS = .
libraries/terminfo_dist-install_DEPS = base-4.5.1.0
libraries/terminfo_dist-install_DEP_NAMES = base
libraries/terminfo_dist-install_INCLUDE_DIRS = 
libraries/terminfo_dist-install_INCLUDES = ncurses.h term.h
libraries/terminfo_dist-install_INSTALL_INCLUDES = 
libraries/terminfo_dist-install_EXTRA_LIBRARIES = ncurses
libraries/terminfo_dist-install_EXTRA_LIBDIRS = 
libraries/terminfo_dist-install_C_SRCS  = 
libraries/terminfo_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard libraries/terminfo/cbits/*.cmm)))
libraries/terminfo_dist-install_DATA_FILES = 
libraries/terminfo_dist-install_HC_OPTS = -Wall -XHaskell98 -XCPP -XForeignFunctionInterface -XDeriveDataTypeable -XEmptyDataDecls -XScopedTypeVariables -XFlexibleInstances
libraries/terminfo_dist-install_CC_OPTS = 
libraries/terminfo_dist-install_CPP_OPTS = 
libraries/terminfo_dist-install_LD_OPTS = 
libraries/terminfo_dist-install_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/base/include' '/home/dieterle/ghc-eden/rts/dist/build' '/home/dieterle/ghc-eden/includes' '/home/dieterle/ghc-eden/includes/dist-ghcconstants/header' '/home/dieterle/ghc-eden/includes/dist-derivedconstants/header'
libraries/terminfo_dist-install_DEP_CC_OPTS = 
libraries/terminfo_dist-install_DEP_LIB_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/base/dist-install/build' '/home/dieterle/ghc-eden/libraries/integer-gmp/dist-install/build' '/home/dieterle/ghc-eden/libraries/ghc-prim/dist-install/build' '/home/dieterle/ghc-eden/rts/dist/build'
libraries/terminfo_dist-install_DEP_EXTRA_LIBS = gmp m rt dl
libraries/terminfo_dist-install_DEP_LD_OPTS = 
libraries/terminfo_dist-install_BUILD_GHCI_LIB = YES

$(eval $(libraries/terminfo_PACKAGE_MAGIC))

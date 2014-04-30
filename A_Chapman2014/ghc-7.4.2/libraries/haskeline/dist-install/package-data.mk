libraries/haskeline_dist-install_VERSION = 0.6.4.0
libraries/haskeline_dist-install_MODULES = System.Console.Haskeline System.Console.Haskeline.Completion System.Console.Haskeline.Encoding System.Console.Haskeline.MonadException System.Console.Haskeline.History System.Console.Haskeline.IO System.Console.Haskeline.Backend.Terminfo System.Console.Haskeline.Backend.Posix System.Console.Haskeline.Backend.IConv System.Console.Haskeline.Backend.DumbTerm System.Console.Haskeline.Backend System.Console.Haskeline.Backend.WCWidth System.Console.Haskeline.Command System.Console.Haskeline.Command.Completion System.Console.Haskeline.Command.History System.Console.Haskeline.Command.KillRing System.Console.Haskeline.Directory System.Console.Haskeline.Emacs System.Console.Haskeline.InputT System.Console.Haskeline.Key System.Console.Haskeline.LineState System.Console.Haskeline.Monads System.Console.Haskeline.Prefs System.Console.Haskeline.RunCommand System.Console.Haskeline.Term System.Console.Haskeline.Command.Undo System.Console.Haskeline.Vi
libraries/haskeline_dist-install_HIDDEN_MODULES = System.Console.Haskeline.Backend.Terminfo System.Console.Haskeline.Backend.Posix System.Console.Haskeline.Backend.IConv System.Console.Haskeline.Backend.DumbTerm System.Console.Haskeline.Backend System.Console.Haskeline.Backend.WCWidth System.Console.Haskeline.Command System.Console.Haskeline.Command.Completion System.Console.Haskeline.Command.History System.Console.Haskeline.Command.KillRing System.Console.Haskeline.Directory System.Console.Haskeline.Emacs System.Console.Haskeline.InputT System.Console.Haskeline.Key System.Console.Haskeline.LineState System.Console.Haskeline.Monads System.Console.Haskeline.Prefs System.Console.Haskeline.RunCommand System.Console.Haskeline.Term System.Console.Haskeline.Command.Undo System.Console.Haskeline.Vi
libraries/haskeline_dist-install_SYNOPSIS =A command-line interface for user input, written in Haskell.
libraries/haskeline_dist-install_HS_SRC_DIRS = .
libraries/haskeline_dist-install_DEPS = base-4.5.1.0 bytestring-0.9.2.1 containers-0.4.2.1 directory-1.1.0.2 extensible-exceptions-0.1.1.4 filepath-1.3.0.0 mtl-1.1.1.1 terminfo-0.3.2.3 unix-2.5.1.1 utf8-string-0.3.8
libraries/haskeline_dist-install_DEP_NAMES = base bytestring containers directory extensible-exceptions filepath mtl terminfo unix utf8-string
libraries/haskeline_dist-install_INCLUDE_DIRS = includes
libraries/haskeline_dist-install_INCLUDES = h_iconv.h
libraries/haskeline_dist-install_INSTALL_INCLUDES = h_iconv.h
libraries/haskeline_dist-install_EXTRA_LIBRARIES = 
libraries/haskeline_dist-install_EXTRA_LIBDIRS = 
libraries/haskeline_dist-install_C_SRCS  = cbits/h_iconv.c cbits/h_wcwidth.c
libraries/haskeline_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard libraries/haskeline/cbits/*.cmm)))
libraries/haskeline_dist-install_DATA_FILES = 
libraries/haskeline_dist-install_HC_OPTS = -Wall -XHaskell98 -XForeignFunctionInterface -XRank2Types -XFlexibleInstances -XTypeSynonymInstances -XFlexibleContexts -XExistentialQuantification -XScopedTypeVariables -XGeneralizedNewtypeDeriving -XMultiParamTypeClasses -XOverlappingInstances -XUndecidableInstances -XPatternSignatures -XCPP -XDeriveDataTypeable -XPatternGuards
libraries/haskeline_dist-install_CC_OPTS = 
libraries/haskeline_dist-install_CPP_OPTS = -DTERMINFO
libraries/haskeline_dist-install_LD_OPTS = 
libraries/haskeline_dist-install_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/directory/include' '/home/dieterle/ghc-eden/libraries/unix/include' '/home/dieterle/ghc-eden/libraries/old-time/include' '/home/dieterle/ghc-eden/libraries/containers/include' '/home/dieterle/ghc-eden/libraries/bytestring/include' '/home/dieterle/ghc-eden/libraries/array/include' '/home/dieterle/ghc-eden/libraries/base/include' '/home/dieterle/ghc-eden/rts/dist/build' '/home/dieterle/ghc-eden/includes' '/home/dieterle/ghc-eden/includes/dist-ghcconstants/header' '/home/dieterle/ghc-eden/includes/dist-derivedconstants/header'
libraries/haskeline_dist-install_DEP_CC_OPTS = 
libraries/haskeline_dist-install_DEP_LIB_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/utf8-string/dist-install/build' '/home/dieterle/ghc-eden/libraries/terminfo/dist-install/build' '/home/dieterle/ghc-eden/libraries/mtl/dist-install/build' '/home/dieterle/ghc-eden/libraries/extensible-exceptions/dist-install/build' '/home/dieterle/ghc-eden/libraries/directory/dist-install/build' '/home/dieterle/ghc-eden/libraries/unix/dist-install/build' '/home/dieterle/ghc-eden/libraries/old-time/dist-install/build' '/home/dieterle/ghc-eden/libraries/old-locale/dist-install/build' '/home/dieterle/ghc-eden/libraries/filepath/dist-install/build' '/home/dieterle/ghc-eden/libraries/containers/dist-install/build' '/home/dieterle/ghc-eden/libraries/deepseq/dist-install/build' '/home/dieterle/ghc-eden/libraries/bytestring/dist-install/build' '/home/dieterle/ghc-eden/libraries/array/dist-install/build' '/home/dieterle/ghc-eden/libraries/base/dist-install/build' '/home/dieterle/ghc-eden/libraries/integer-gmp/dist-install/build' '/home/dieterle/ghc-eden/libraries/ghc-prim/dist-install/build' '/home/dieterle/ghc-eden/rts/dist/build'
libraries/haskeline_dist-install_DEP_EXTRA_LIBS = ncurses rt util dl pthread gmp m rt dl
libraries/haskeline_dist-install_DEP_LD_OPTS = 
libraries/haskeline_dist-install_BUILD_GHCI_LIB = YES

$(eval $(libraries/haskeline_PACKAGE_MAGIC))

utils/ghc-pkg_dist-install_VERSION = 6.9
utils/ghc-pkg_dist-install_MODULES = Main Version
utils/ghc-pkg_dist-install_HIDDEN_MODULES = Version
utils/ghc-pkg_dist-install_SYNOPSIS =XXX
utils/ghc-pkg_dist-install_HS_SRC_DIRS = .
utils/ghc-pkg_dist-install_DEPS = Cabal-1.14.0 base-4.5.1.0 bin-package-db-0.0.0.0 binary-0.5.1.0 bytestring-0.9.2.1 directory-1.1.0.2 filepath-1.3.0.0 process-1.1.0.1 terminfo-0.3.2.3 unix-2.5.1.1
utils/ghc-pkg_dist-install_DEP_NAMES = Cabal base bin-package-db binary bytestring directory filepath process terminfo unix
utils/ghc-pkg_dist-install_INCLUDE_DIRS = 
utils/ghc-pkg_dist-install_INCLUDES = 
utils/ghc-pkg_dist-install_INSTALL_INCLUDES = 
utils/ghc-pkg_dist-install_EXTRA_LIBRARIES = 
utils/ghc-pkg_dist-install_EXTRA_LIBDIRS = 
utils/ghc-pkg_dist-install_C_SRCS  = 
utils/ghc-pkg_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard utils/ghc-pkg/cbits/*.cmm)))
utils/ghc-pkg_dist-install_DATA_FILES = 
utils/ghc-pkg_dist-install_HC_OPTS = -XHaskell98 -XCPP -XForeignFunctionInterface -XNondecreasingIndentation
utils/ghc-pkg_dist-install_CC_OPTS = 
utils/ghc-pkg_dist-install_CPP_OPTS = 
utils/ghc-pkg_dist-install_LD_OPTS = 
utils/ghc-pkg_dist-install_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/process/include' '/home/dieterle/ghc-eden/libraries/directory/include' '/home/dieterle/ghc-eden/libraries/unix/include' '/home/dieterle/ghc-eden/libraries/bytestring/include' '/home/dieterle/ghc-eden/libraries/old-time/include' '/home/dieterle/ghc-eden/libraries/containers/include' '/home/dieterle/ghc-eden/libraries/array/include' '/home/dieterle/ghc-eden/libraries/base/include' '/home/dieterle/ghc-eden/rts/dist/build' '/home/dieterle/ghc-eden/includes' '/home/dieterle/ghc-eden/includes/dist-ghcconstants/header' '/home/dieterle/ghc-eden/includes/dist-derivedconstants/header'
utils/ghc-pkg_dist-install_DEP_CC_OPTS = 
utils/ghc-pkg_dist-install_DEP_LIB_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/terminfo/dist-install/build' '/home/dieterle/ghc-eden/libraries/bin-package-db/dist-install/build' '/home/dieterle/ghc-eden/libraries/binary/dist-install/build' '/home/dieterle/ghc-eden/libraries/Cabal/Cabal/dist-install/build' '/home/dieterle/ghc-eden/libraries/process/dist-install/build' '/home/dieterle/ghc-eden/libraries/pretty/dist-install/build' '/home/dieterle/ghc-eden/libraries/directory/dist-install/build' '/home/dieterle/ghc-eden/libraries/unix/dist-install/build' '/home/dieterle/ghc-eden/libraries/bytestring/dist-install/build' '/home/dieterle/ghc-eden/libraries/old-time/dist-install/build' '/home/dieterle/ghc-eden/libraries/old-locale/dist-install/build' '/home/dieterle/ghc-eden/libraries/filepath/dist-install/build' '/home/dieterle/ghc-eden/libraries/containers/dist-install/build' '/home/dieterle/ghc-eden/libraries/deepseq/dist-install/build' '/home/dieterle/ghc-eden/libraries/array/dist-install/build' '/home/dieterle/ghc-eden/libraries/base/dist-install/build' '/home/dieterle/ghc-eden/libraries/integer-gmp/dist-install/build' '/home/dieterle/ghc-eden/libraries/ghc-prim/dist-install/build' '/home/dieterle/ghc-eden/rts/dist/build'
utils/ghc-pkg_dist-install_DEP_EXTRA_LIBS = ncurses rt util dl pthread gmp m rt dl
utils/ghc-pkg_dist-install_DEP_LD_OPTS = 
utils/ghc-pkg_dist-install_BUILD_GHCI_LIB = YES

$(eval $(utils/ghc-pkg_PACKAGE_MAGIC))

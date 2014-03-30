libraries/unix_dist-install_VERSION = 2.5.1.1
libraries/unix_dist-install_MODULES = System.Posix System.Posix.ByteString System.Posix.Error System.Posix.Resource System.Posix.Time System.Posix.Unistd System.Posix.User System.Posix.Signals System.Posix.Signals.Exts System.Posix.Semaphore System.Posix.SharedMem System.Posix.ByteString.FilePath System.Posix.Directory System.Posix.Directory.ByteString System.Posix.DynamicLinker.Module System.Posix.DynamicLinker.Module.ByteString System.Posix.DynamicLinker.Prim System.Posix.DynamicLinker.ByteString System.Posix.DynamicLinker System.Posix.Files System.Posix.Files.ByteString System.Posix.IO System.Posix.IO.ByteString System.Posix.Env System.Posix.Env.ByteString System.Posix.Process System.Posix.Process.Internals System.Posix.Process.ByteString System.Posix.Temp System.Posix.Temp.ByteString System.Posix.Terminal System.Posix.Terminal.ByteString System.Posix.Directory.Common System.Posix.DynamicLinker.Common System.Posix.Files.Common System.Posix.IO.Common System.Posix.Process.Common System.Posix.Terminal.Common
libraries/unix_dist-install_HIDDEN_MODULES = System.Posix.Directory.Common System.Posix.DynamicLinker.Common System.Posix.Files.Common System.Posix.IO.Common System.Posix.Process.Common System.Posix.Terminal.Common
libraries/unix_dist-install_SYNOPSIS =POSIX functionality
libraries/unix_dist-install_HS_SRC_DIRS = .
libraries/unix_dist-install_DEPS = base-4.5.1.0 bytestring-0.9.2.1
libraries/unix_dist-install_DEP_NAMES = base bytestring
libraries/unix_dist-install_INCLUDE_DIRS = include
libraries/unix_dist-install_INCLUDES = HsUnix.h execvpe.h
libraries/unix_dist-install_INSTALL_INCLUDES = HsUnix.h HsUnixConfig.h execvpe.h
libraries/unix_dist-install_EXTRA_LIBRARIES = rt util dl pthread
libraries/unix_dist-install_EXTRA_LIBDIRS = 
libraries/unix_dist-install_C_SRCS  = cbits/HsUnix.c cbits/execvpe.c cbits/dirUtils.c
libraries/unix_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard libraries/unix/cbits/*.cmm)))
libraries/unix_dist-install_DATA_FILES = 
libraries/unix_dist-install_HC_OPTS = -XHaskell98 -XNondecreasingIndentation -XCPP -XForeignFunctionInterface -XEmptyDataDecls
libraries/unix_dist-install_CC_OPTS = 
libraries/unix_dist-install_CPP_OPTS = 
libraries/unix_dist-install_LD_OPTS = 
libraries/unix_dist-install_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/bytestring/include' '/home/dieterle/ghc-eden/libraries/base/include' '/home/dieterle/ghc-eden/rts/dist/build' '/home/dieterle/ghc-eden/includes' '/home/dieterle/ghc-eden/includes/dist-ghcconstants/header' '/home/dieterle/ghc-eden/includes/dist-derivedconstants/header'
libraries/unix_dist-install_DEP_CC_OPTS = 
libraries/unix_dist-install_DEP_LIB_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/bytestring/dist-install/build' '/home/dieterle/ghc-eden/libraries/base/dist-install/build' '/home/dieterle/ghc-eden/libraries/integer-gmp/dist-install/build' '/home/dieterle/ghc-eden/libraries/ghc-prim/dist-install/build' '/home/dieterle/ghc-eden/rts/dist/build'
libraries/unix_dist-install_DEP_EXTRA_LIBS = gmp m rt dl
libraries/unix_dist-install_DEP_LD_OPTS = 
libraries/unix_dist-install_BUILD_GHCI_LIB = YES

$(eval $(libraries/unix_PACKAGE_MAGIC))

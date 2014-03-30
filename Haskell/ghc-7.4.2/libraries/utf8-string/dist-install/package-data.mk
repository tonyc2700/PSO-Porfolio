libraries/utf8-string_dist-install_VERSION = 0.3.8
libraries/utf8-string_dist-install_MODULES = Codec.Binary.UTF8.String Codec.Binary.UTF8.Generic System.IO.UTF8 System.Environment.UTF8 Data.String.UTF8 Data.ByteString.UTF8 Data.ByteString.Lazy.UTF8
libraries/utf8-string_dist-install_HIDDEN_MODULES = 
libraries/utf8-string_dist-install_SYNOPSIS =Support for reading and writing UTF8 Strings
libraries/utf8-string_dist-install_HS_SRC_DIRS = .
libraries/utf8-string_dist-install_DEPS = base-4.5.1.0 bytestring-0.9.2.1
libraries/utf8-string_dist-install_DEP_NAMES = base bytestring
libraries/utf8-string_dist-install_INCLUDE_DIRS = 
libraries/utf8-string_dist-install_INCLUDES = 
libraries/utf8-string_dist-install_INSTALL_INCLUDES = 
libraries/utf8-string_dist-install_EXTRA_LIBRARIES = 
libraries/utf8-string_dist-install_EXTRA_LIBDIRS = 
libraries/utf8-string_dist-install_C_SRCS  = 
libraries/utf8-string_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard libraries/utf8-string/cbits/*.cmm)))
libraries/utf8-string_dist-install_DATA_FILES = 
libraries/utf8-string_dist-install_HC_OPTS = -W -O2 -XHaskell98 -XCPP
libraries/utf8-string_dist-install_CC_OPTS = 
libraries/utf8-string_dist-install_CPP_OPTS = 
libraries/utf8-string_dist-install_LD_OPTS = 
libraries/utf8-string_dist-install_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/bytestring/include' '/home/dieterle/ghc-eden/libraries/base/include' '/home/dieterle/ghc-eden/rts/dist/build' '/home/dieterle/ghc-eden/includes' '/home/dieterle/ghc-eden/includes/dist-ghcconstants/header' '/home/dieterle/ghc-eden/includes/dist-derivedconstants/header'
libraries/utf8-string_dist-install_DEP_CC_OPTS = 
libraries/utf8-string_dist-install_DEP_LIB_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/bytestring/dist-install/build' '/home/dieterle/ghc-eden/libraries/base/dist-install/build' '/home/dieterle/ghc-eden/libraries/integer-gmp/dist-install/build' '/home/dieterle/ghc-eden/libraries/ghc-prim/dist-install/build' '/home/dieterle/ghc-eden/rts/dist/build'
libraries/utf8-string_dist-install_DEP_EXTRA_LIBS = gmp m rt dl
libraries/utf8-string_dist-install_DEP_LD_OPTS = 
libraries/utf8-string_dist-install_BUILD_GHCI_LIB = YES

$(eval $(libraries/utf8-string_PACKAGE_MAGIC))

libraries/base_dist-install_VERSION = 4.5.1.0
libraries/base_dist-install_MODULES = Foreign.Concurrent GHC.Arr GHC.Base GHC.Conc GHC.Conc.IO GHC.Conc.Signal GHC.Conc.Sync GHC.ConsoleHandler GHC.Constants GHC.Desugar GHC.Enum GHC.Environment GHC.Err GHC.Exception GHC.Exts GHC.Fingerprint GHC.Fingerprint.Type GHC.Float GHC.Float.ConversionUtils GHC.Float.RealFracMethods GHC.Foreign GHC.ForeignPtr GHC.Handle GHC.IO GHC.IO.Buffer GHC.IO.BufferedIO GHC.IO.Device GHC.IO.Encoding GHC.IO.Encoding.CodePage GHC.IO.Encoding.Failure GHC.IO.Encoding.Iconv GHC.IO.Encoding.Latin1 GHC.IO.Encoding.Types GHC.IO.Encoding.UTF16 GHC.IO.Encoding.UTF32 GHC.IO.Encoding.UTF8 GHC.IO.Exception GHC.IO.FD GHC.IO.Handle GHC.IO.Handle.FD GHC.IO.Handle.Internals GHC.IO.Handle.Text GHC.IO.Handle.Types GHC.IO.IOMode GHC.IOArray GHC.IOBase GHC.IORef GHC.Int GHC.List GHC.MVar GHC.Num GHC.PArr GHC.Pack GHC.Ptr GHC.Read GHC.Real GHC.ST GHC.Stack GHC.Stats GHC.Show GHC.Stable GHC.Storable GHC.STRef GHC.TopHandler GHC.Unicode GHC.Weak GHC.Word System.Timeout GHC.Event Control.Applicative Control.Arrow Control.Category Control.Concurrent Control.Concurrent.Chan Control.Concurrent.MVar Control.Concurrent.QSem Control.Concurrent.QSemN Control.Concurrent.SampleVar Control.Exception Control.Exception.Base Control.OldException Control.Monad Control.Monad.Fix Control.Monad.Instances Control.Monad.ST Control.Monad.ST.Safe Control.Monad.ST.Unsafe Control.Monad.ST.Lazy Control.Monad.ST.Lazy.Safe Control.Monad.ST.Lazy.Unsafe Control.Monad.ST.Strict Control.Monad.Zip Data.Bits Data.Bool Data.Char Data.Complex Data.Dynamic Data.Either Data.Eq Data.Data Data.Fixed Data.Foldable Data.Function Data.Functor Data.HashTable Data.IORef Data.Int Data.Ix Data.List Data.Maybe Data.Monoid Data.Ord Data.Ratio Data.STRef Data.STRef.Lazy Data.STRef.Strict Data.String Data.Traversable Data.Tuple Data.Typeable Data.Typeable.Internal Data.Unique Data.Version Data.Word Debug.Trace Foreign Foreign.C Foreign.C.Error Foreign.C.String Foreign.C.Types Foreign.ForeignPtr Foreign.ForeignPtr.Safe Foreign.ForeignPtr.Unsafe Foreign.Marshal Foreign.Marshal.Alloc Foreign.Marshal.Array Foreign.Marshal.Error Foreign.Marshal.Pool Foreign.Marshal.Safe Foreign.Marshal.Utils Foreign.Marshal.Unsafe Foreign.Ptr Foreign.Safe Foreign.StablePtr Foreign.Storable Numeric Prelude System.Console.GetOpt System.CPUTime System.Environment System.Exit System.IO System.IO.Error System.IO.Unsafe System.Info System.Mem System.Mem.StableName System.Mem.Weak System.Posix.Internals System.Posix.Types Text.ParserCombinators.ReadP Text.ParserCombinators.ReadPrec Text.Printf Text.Read Text.Read.Lex Text.Show Text.Show.Functions Unsafe.Coerce GHC.Event.Array GHC.Event.Clock GHC.Event.Control GHC.Event.EPoll GHC.Event.IntMap GHC.Event.Internal GHC.Event.KQueue GHC.Event.Manager GHC.Event.PSQ GHC.Event.Poll GHC.Event.Thread GHC.Event.Unique Control.Monad.ST.Imp Control.Monad.ST.Lazy.Imp Foreign.ForeignPtr.Imp
libraries/base_dist-install_HIDDEN_MODULES = GHC.Event.Array GHC.Event.Clock GHC.Event.Control GHC.Event.EPoll GHC.Event.IntMap GHC.Event.Internal GHC.Event.KQueue GHC.Event.Manager GHC.Event.PSQ GHC.Event.Poll GHC.Event.Thread GHC.Event.Unique Control.Monad.ST.Imp Control.Monad.ST.Lazy.Imp Foreign.ForeignPtr.Imp
libraries/base_dist-install_SYNOPSIS =Basic libraries
libraries/base_dist-install_HS_SRC_DIRS = .
libraries/base_dist-install_DEPS = ghc-prim-0.2.0.0 integer-gmp-0.4.0.0 rts-1.0
libraries/base_dist-install_DEP_NAMES = ghc-prim integer-gmp rts
libraries/base_dist-install_INCLUDE_DIRS = include
libraries/base_dist-install_INCLUDES = HsBase.h
libraries/base_dist-install_INSTALL_INCLUDES = HsBase.h HsBaseConfig.h EventConfig.h WCsubst.h consUtils.h Typeable.h
libraries/base_dist-install_EXTRA_LIBRARIES = 
libraries/base_dist-install_EXTRA_LIBDIRS = 
libraries/base_dist-install_C_SRCS  = cbits/PrelIOUtils.c cbits/WCsubst.c cbits/Win32Utils.c cbits/consUtils.c cbits/iconv.c cbits/inputReady.c cbits/selectUtils.c cbits/primFloat.c cbits/md5.c
libraries/base_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard libraries/base/cbits/*.cmm)))
libraries/base_dist-install_DATA_FILES = 
libraries/base_dist-install_HC_OPTS = -package-name base -XHaskell98 -XCPP
libraries/base_dist-install_CC_OPTS = 
libraries/base_dist-install_CPP_OPTS = -DOPTIMISE_INTEGER_GCD_LCM
libraries/base_dist-install_LD_OPTS = 
libraries/base_dist-install_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/rts/dist/build' '/home/dieterle/ghc-eden/includes' '/home/dieterle/ghc-eden/includes/dist-ghcconstants/header' '/home/dieterle/ghc-eden/includes/dist-derivedconstants/header'
libraries/base_dist-install_DEP_CC_OPTS = 
libraries/base_dist-install_DEP_LIB_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/integer-gmp/dist-install/build' '/home/dieterle/ghc-eden/libraries/ghc-prim/dist-install/build' '/home/dieterle/ghc-eden/rts/dist/build'
libraries/base_dist-install_DEP_EXTRA_LIBS = gmp m rt dl
libraries/base_dist-install_DEP_LD_OPTS = 
libraries/base_dist-install_BUILD_GHCI_LIB = YES

$(eval $(libraries/base_PACKAGE_MAGIC))

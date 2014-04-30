libraries/Cabal/Cabal_dist-boot_VERSION = 1.14.0
libraries/Cabal/Cabal_dist-boot_MODULES = Distribution.Compiler Distribution.InstalledPackageInfo Distribution.License Distribution.Make Distribution.ModuleName Distribution.Package Distribution.PackageDescription Distribution.PackageDescription.Configuration Distribution.PackageDescription.Parse Distribution.PackageDescription.Check Distribution.PackageDescription.PrettyPrint Distribution.ParseUtils Distribution.ReadE Distribution.Simple Distribution.Simple.Build Distribution.Simple.Build.Macros Distribution.Simple.Build.PathsModule Distribution.Simple.BuildPaths Distribution.Simple.Bench Distribution.Simple.Command Distribution.Simple.Compiler Distribution.Simple.Configure Distribution.Simple.GHC Distribution.Simple.LHC Distribution.Simple.Haddock Distribution.Simple.Hpc Distribution.Simple.Hugs Distribution.Simple.Install Distribution.Simple.InstallDirs Distribution.Simple.JHC Distribution.Simple.LocalBuildInfo Distribution.Simple.NHC Distribution.Simple.PackageIndex Distribution.Simple.PreProcess Distribution.Simple.PreProcess.Unlit Distribution.Simple.Program Distribution.Simple.Program.Ar Distribution.Simple.Program.Builtin Distribution.Simple.Program.Db Distribution.Simple.Program.HcPkg Distribution.Simple.Program.Hpc Distribution.Simple.Program.Ld Distribution.Simple.Program.Run Distribution.Simple.Program.Script Distribution.Simple.Program.Types Distribution.Simple.Register Distribution.Simple.Setup Distribution.Simple.SrcDist Distribution.Simple.Test Distribution.Simple.UHC Distribution.Simple.UserHooks Distribution.Simple.Utils Distribution.System Distribution.TestSuite Distribution.Text Distribution.Verbosity Distribution.Version Distribution.Compat.ReadP Language.Haskell.Extension Distribution.GetOpt Distribution.Compat.Exception Distribution.Compat.CopyFile Distribution.Compat.TempFile Distribution.Simple.GHC.IPI641 Distribution.Simple.GHC.IPI642 Paths_Cabal
libraries/Cabal/Cabal_dist-boot_HIDDEN_MODULES = Distribution.GetOpt Distribution.Compat.Exception Distribution.Compat.CopyFile Distribution.Compat.TempFile Distribution.Simple.GHC.IPI641 Distribution.Simple.GHC.IPI642 Paths_Cabal
libraries/Cabal/Cabal_dist-boot_SYNOPSIS =A framework for packaging Haskell software
libraries/Cabal/Cabal_dist-boot_HS_SRC_DIRS = .
libraries/Cabal/Cabal_dist-boot_DEPS = array-0.3.0.2 base-4.3.1.0 containers-0.4.0.0 directory-1.1.0.0 filepath-1.2.0.0 old-time-1.0.0.6 pretty-1.0.1.2 process-1.0.1.5 unix-2.4.2.0
libraries/Cabal/Cabal_dist-boot_DEP_NAMES = array base containers directory filepath old-time pretty process unix
libraries/Cabal/Cabal_dist-boot_INCLUDE_DIRS = 
libraries/Cabal/Cabal_dist-boot_INCLUDES = 
libraries/Cabal/Cabal_dist-boot_INSTALL_INCLUDES = 
libraries/Cabal/Cabal_dist-boot_EXTRA_LIBRARIES = 
libraries/Cabal/Cabal_dist-boot_EXTRA_LIBDIRS = 
libraries/Cabal/Cabal_dist-boot_C_SRCS  = 
libraries/Cabal/Cabal_dist-boot_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard libraries/Cabal/Cabal/cbits/*.cmm)))
libraries/Cabal/Cabal_dist-boot_DATA_FILES = 
libraries/Cabal/Cabal_dist-boot_HC_OPTS = -fwarn-tabs -Wall -fno-ignore-asserts -XHaskell98 -XCPP
libraries/Cabal/Cabal_dist-boot_CC_OPTS = 
libraries/Cabal/Cabal_dist-boot_CPP_OPTS = 
libraries/Cabal/Cabal_dist-boot_LD_OPTS = 
libraries/Cabal/Cabal_dist-boot_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/usr/lib/ghc-7.0.3/process-1.0.1.5/include' '/usr/lib/ghc-7.0.3/directory-1.1.0.0/include' '/usr/lib/ghc-7.0.3/unix-2.4.2.0/include' '/usr/lib/ghc-7.0.3/old-time-1.0.0.6/include' '/usr/lib/ghc-7.0.3/base-4.3.1.0/include' '/usr/lib/ghc-7.0.3/include'
libraries/Cabal/Cabal_dist-boot_DEP_CC_OPTS = 
libraries/Cabal/Cabal_dist-boot_DEP_LIB_DIRS_SINGLE_QUOTED = '/usr/lib/ghc-7.0.3/process-1.0.1.5' '/usr/lib/ghc-7.0.3/pretty-1.0.1.2' '/usr/lib/ghc-7.0.3/directory-1.1.0.0' '/usr/lib/ghc-7.0.3/unix-2.4.2.0' '/usr/lib/ghc-7.0.3/old-time-1.0.0.6' '/usr/lib/ghc-7.0.3/old-locale-1.0.0.2' '/usr/lib/ghc-7.0.3/filepath-1.2.0.0' '/usr/lib/ghc-7.0.3/containers-0.4.0.0' '/usr/lib/ghc-7.0.3/array-0.3.0.2' '/usr/lib/ghc-7.0.3/base-4.3.1.0' '/usr/lib/ghc-7.0.3/integer-gmp-0.2.0.3' '/usr/lib/ghc-7.0.3/ghc-prim-0.2.0.0' '/usr/lib/ghc-7.0.3'
libraries/Cabal/Cabal_dist-boot_DEP_EXTRA_LIBS = rt util dl pthread gmp ffi m rt dl
libraries/Cabal/Cabal_dist-boot_DEP_LD_OPTS = 
libraries/Cabal/Cabal_dist-boot_BUILD_GHCI_LIB = YES

$(eval $(libraries/Cabal/Cabal_PACKAGE_MAGIC))

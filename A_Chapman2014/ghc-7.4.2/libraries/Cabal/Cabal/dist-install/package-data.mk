libraries/Cabal/Cabal_dist-install_VERSION = 1.14.0
libraries/Cabal/Cabal_dist-install_MODULES = Distribution.Compiler Distribution.InstalledPackageInfo Distribution.License Distribution.Make Distribution.ModuleName Distribution.Package Distribution.PackageDescription Distribution.PackageDescription.Configuration Distribution.PackageDescription.Parse Distribution.PackageDescription.Check Distribution.PackageDescription.PrettyPrint Distribution.ParseUtils Distribution.ReadE Distribution.Simple Distribution.Simple.Build Distribution.Simple.Build.Macros Distribution.Simple.Build.PathsModule Distribution.Simple.BuildPaths Distribution.Simple.Bench Distribution.Simple.Command Distribution.Simple.Compiler Distribution.Simple.Configure Distribution.Simple.GHC Distribution.Simple.LHC Distribution.Simple.Haddock Distribution.Simple.Hpc Distribution.Simple.Hugs Distribution.Simple.Install Distribution.Simple.InstallDirs Distribution.Simple.JHC Distribution.Simple.LocalBuildInfo Distribution.Simple.NHC Distribution.Simple.PackageIndex Distribution.Simple.PreProcess Distribution.Simple.PreProcess.Unlit Distribution.Simple.Program Distribution.Simple.Program.Ar Distribution.Simple.Program.Builtin Distribution.Simple.Program.Db Distribution.Simple.Program.HcPkg Distribution.Simple.Program.Hpc Distribution.Simple.Program.Ld Distribution.Simple.Program.Run Distribution.Simple.Program.Script Distribution.Simple.Program.Types Distribution.Simple.Register Distribution.Simple.Setup Distribution.Simple.SrcDist Distribution.Simple.Test Distribution.Simple.UHC Distribution.Simple.UserHooks Distribution.Simple.Utils Distribution.System Distribution.TestSuite Distribution.Text Distribution.Verbosity Distribution.Version Distribution.Compat.ReadP Language.Haskell.Extension Distribution.GetOpt Distribution.Compat.Exception Distribution.Compat.CopyFile Distribution.Compat.TempFile Distribution.Simple.GHC.IPI641 Distribution.Simple.GHC.IPI642 Paths_Cabal
libraries/Cabal/Cabal_dist-install_HIDDEN_MODULES = Distribution.GetOpt Distribution.Compat.Exception Distribution.Compat.CopyFile Distribution.Compat.TempFile Distribution.Simple.GHC.IPI641 Distribution.Simple.GHC.IPI642 Paths_Cabal
libraries/Cabal/Cabal_dist-install_SYNOPSIS =A framework for packaging Haskell software
libraries/Cabal/Cabal_dist-install_HS_SRC_DIRS = .
libraries/Cabal/Cabal_dist-install_DEPS = array-0.4.0.0 base-4.5.1.0 containers-0.4.2.1 directory-1.1.0.2 filepath-1.3.0.0 old-time-1.1.0.0 pretty-1.1.1.0 process-1.1.0.1 unix-2.5.1.1
libraries/Cabal/Cabal_dist-install_DEP_NAMES = array base containers directory filepath old-time pretty process unix
libraries/Cabal/Cabal_dist-install_INCLUDE_DIRS = 
libraries/Cabal/Cabal_dist-install_INCLUDES = 
libraries/Cabal/Cabal_dist-install_INSTALL_INCLUDES = 
libraries/Cabal/Cabal_dist-install_EXTRA_LIBRARIES = 
libraries/Cabal/Cabal_dist-install_EXTRA_LIBDIRS = 
libraries/Cabal/Cabal_dist-install_C_SRCS  = 
libraries/Cabal/Cabal_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard libraries/Cabal/Cabal/cbits/*.cmm)))
libraries/Cabal/Cabal_dist-install_DATA_FILES = 
libraries/Cabal/Cabal_dist-install_HC_OPTS = -fwarn-tabs -Wall -fno-ignore-asserts -XHaskell98 -XCPP
libraries/Cabal/Cabal_dist-install_CC_OPTS = 
libraries/Cabal/Cabal_dist-install_CPP_OPTS = 
libraries/Cabal/Cabal_dist-install_LD_OPTS = 
libraries/Cabal/Cabal_dist-install_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/process/include' '/home/dieterle/ghc-eden/libraries/directory/include' '/home/dieterle/ghc-eden/libraries/unix/include' '/home/dieterle/ghc-eden/libraries/old-time/include' '/home/dieterle/ghc-eden/libraries/containers/include' '/home/dieterle/ghc-eden/libraries/bytestring/include' '/home/dieterle/ghc-eden/libraries/array/include' '/home/dieterle/ghc-eden/libraries/base/include' '/home/dieterle/ghc-eden/rts/dist/build' '/home/dieterle/ghc-eden/includes' '/home/dieterle/ghc-eden/includes/dist-ghcconstants/header' '/home/dieterle/ghc-eden/includes/dist-derivedconstants/header'
libraries/Cabal/Cabal_dist-install_DEP_CC_OPTS = 
libraries/Cabal/Cabal_dist-install_DEP_LIB_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/process/dist-install/build' '/home/dieterle/ghc-eden/libraries/pretty/dist-install/build' '/home/dieterle/ghc-eden/libraries/directory/dist-install/build' '/home/dieterle/ghc-eden/libraries/unix/dist-install/build' '/home/dieterle/ghc-eden/libraries/old-time/dist-install/build' '/home/dieterle/ghc-eden/libraries/old-locale/dist-install/build' '/home/dieterle/ghc-eden/libraries/filepath/dist-install/build' '/home/dieterle/ghc-eden/libraries/containers/dist-install/build' '/home/dieterle/ghc-eden/libraries/deepseq/dist-install/build' '/home/dieterle/ghc-eden/libraries/bytestring/dist-install/build' '/home/dieterle/ghc-eden/libraries/array/dist-install/build' '/home/dieterle/ghc-eden/libraries/base/dist-install/build' '/home/dieterle/ghc-eden/libraries/integer-gmp/dist-install/build' '/home/dieterle/ghc-eden/libraries/ghc-prim/dist-install/build' '/home/dieterle/ghc-eden/rts/dist/build'
libraries/Cabal/Cabal_dist-install_DEP_EXTRA_LIBS = rt util dl pthread gmp m rt dl
libraries/Cabal/Cabal_dist-install_DEP_LD_OPTS = 
libraries/Cabal/Cabal_dist-install_BUILD_GHCI_LIB = YES

$(eval $(libraries/Cabal/Cabal_PACKAGE_MAGIC))

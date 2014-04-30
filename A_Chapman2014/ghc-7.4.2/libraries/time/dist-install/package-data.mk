libraries/time_dist-install_VERSION = 1.4
libraries/time_dist-install_MODULES = Data.Time.Calendar Data.Time.Calendar.MonthDay Data.Time.Calendar.OrdinalDate Data.Time.Calendar.WeekDate Data.Time.Calendar.Julian Data.Time.Calendar.Easter Data.Time.Clock Data.Time.Clock.POSIX Data.Time.Clock.TAI Data.Time.LocalTime Data.Time.Format Data.Time Data.Time.Calendar.Private Data.Time.Calendar.Days Data.Time.Calendar.Gregorian Data.Time.Calendar.JulianYearDay Data.Time.Clock.Scale Data.Time.Clock.UTC Data.Time.Clock.CTimeval Data.Time.Clock.UTCDiff Data.Time.LocalTime.TimeZone Data.Time.LocalTime.TimeOfDay Data.Time.LocalTime.LocalTime Data.Time.Format.Parse
libraries/time_dist-install_HIDDEN_MODULES = Data.Time.Calendar.Private Data.Time.Calendar.Days Data.Time.Calendar.Gregorian Data.Time.Calendar.JulianYearDay Data.Time.Clock.Scale Data.Time.Clock.UTC Data.Time.Clock.CTimeval Data.Time.Clock.UTCDiff Data.Time.LocalTime.TimeZone Data.Time.LocalTime.TimeOfDay Data.Time.LocalTime.LocalTime Data.Time.Format.Parse
libraries/time_dist-install_SYNOPSIS =A time library
libraries/time_dist-install_HS_SRC_DIRS = .
libraries/time_dist-install_DEPS = base-4.5.1.0 deepseq-1.3.0.0 old-locale-1.0.0.4
libraries/time_dist-install_DEP_NAMES = base deepseq old-locale
libraries/time_dist-install_INCLUDE_DIRS = include
libraries/time_dist-install_INCLUDES = 
libraries/time_dist-install_INSTALL_INCLUDES = HsTime.h HsTimeConfig.h
libraries/time_dist-install_EXTRA_LIBRARIES = 
libraries/time_dist-install_EXTRA_LIBDIRS = 
libraries/time_dist-install_C_SRCS  = cbits/HsTime.c
libraries/time_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir $(wildcard libraries/time/cbits/*.cmm)))
libraries/time_dist-install_DATA_FILES = 
libraries/time_dist-install_HC_OPTS = -Wall -XHaskell98 -XRank2Types -XDeriveDataTypeable -XStandaloneDeriving -XForeignFunctionInterface -XCPP
libraries/time_dist-install_CC_OPTS = 
libraries/time_dist-install_CPP_OPTS = -DLANGUAGE_Rank2Types -DLANGUAGE_DeriveDataTypeable -DLANGUAGE_StandaloneDeriving
libraries/time_dist-install_LD_OPTS = 
libraries/time_dist-install_DEP_INCLUDE_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/array/include' '/home/dieterle/ghc-eden/libraries/base/include' '/home/dieterle/ghc-eden/rts/dist/build' '/home/dieterle/ghc-eden/includes' '/home/dieterle/ghc-eden/includes/dist-ghcconstants/header' '/home/dieterle/ghc-eden/includes/dist-derivedconstants/header'
libraries/time_dist-install_DEP_CC_OPTS = 
libraries/time_dist-install_DEP_LIB_DIRS_SINGLE_QUOTED = '/home/dieterle/ghc-eden/libraries/old-locale/dist-install/build' '/home/dieterle/ghc-eden/libraries/deepseq/dist-install/build' '/home/dieterle/ghc-eden/libraries/array/dist-install/build' '/home/dieterle/ghc-eden/libraries/base/dist-install/build' '/home/dieterle/ghc-eden/libraries/integer-gmp/dist-install/build' '/home/dieterle/ghc-eden/libraries/ghc-prim/dist-install/build' '/home/dieterle/ghc-eden/rts/dist/build'
libraries/time_dist-install_DEP_EXTRA_LIBS = gmp m rt dl
libraries/time_dist-install_DEP_LD_OPTS = 
libraries/time_dist-install_BUILD_GHCI_LIB = YES

$(eval $(libraries/time_PACKAGE_MAGIC))

# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# For expressing extra dependencies on source files

define compiler-hs-dependency # args: $1 = module, $2 = dependency

$$(foreach stage,1 2 3,\
 $$(foreach way,$$(compiler_stage$$(stage)_WAYS),\
  compiler/stage$$(stage)/build/$1.$$($$(way)_osuf))) : $2

endef

# -----------------------------------------------------------------------------
# Create compiler configuration
#
# The 'echo' commands simply spit the values of various make variables
# into Config.hs, whence they can be compiled and used by GHC itself

compiler_CONFIG_HS = compiler/main/Config.hs

# This is just to avoid generating a warning when generating deps
# involving RtsFlags.h
compiler_stage1_MKDEPENDC_OPTS = -DMAKING_GHC_BUILD_SYSTEM_DEPENDENCIES
compiler_stage2_MKDEPENDC_OPTS = -DMAKING_GHC_BUILD_SYSTEM_DEPENDENCIES
compiler_stage3_MKDEPENDC_OPTS = -DMAKING_GHC_BUILD_SYSTEM_DEPENDENCIES

compiler_stage1_C_FILES_NODEPS = compiler/parser/cutils.c

ifneq "$(BINDIST)" "YES"
compiler/stage1/package-data.mk : compiler/stage1/build/Config.hs
compiler/stage2/package-data.mk : compiler/stage2/build/Config.hs
compiler/stage3/package-data.mk : compiler/stage3/build/Config.hs
endif

compiler/stage%/build/Config.hs : mk/config.mk mk/project.mk | $$(dir $$@)/.
	$(call removeFiles,$@)
	@echo 'Creating $@ ... '
	@echo '{-# LANGUAGE CPP #-}'                                        >> $@
	@echo 'module Config where'                                         >> $@
	@echo                                                               >> $@
	@echo '#include "ghc_boot_platform.h"'                              >> $@
	@echo                                                               >> $@
	@echo 'data IntegerLibrary = IntegerGMP | IntegerSimple'            >> $@
	@echo '    deriving Eq'                                             >> $@
	@echo                                                               >> $@
	@echo 'cBuildPlatformString :: String'                              >> $@
	@echo 'cBuildPlatformString = BuildPlatform_NAME'                   >> $@
	@echo 'cHostPlatformString :: String'                               >> $@
	@echo 'cHostPlatformString = HostPlatform_NAME'                     >> $@
	@echo 'cTargetPlatformString :: String'                             >> $@
	@echo 'cTargetPlatformString = TargetPlatform_NAME'                 >> $@
	@echo                                                               >> $@
	@echo 'cProjectName          :: String'                             >> $@
	@echo 'cProjectName          = "$(ProjectName)"'                    >> $@
	@echo 'cProjectVersion       :: String'                             >> $@
	@echo 'cProjectVersion       = "$(ProjectVersion)"'                 >> $@
	@echo 'cProjectVersionInt    :: String'                             >> $@
	@echo 'cProjectVersionInt    = "$(ProjectVersionInt)"'              >> $@
	@echo 'cProjectPatchLevel    :: String'                             >> $@
	@echo 'cProjectPatchLevel    = "$(ProjectPatchLevel)"'              >> $@
	@echo 'cBooterVersion        :: String'                             >> $@
	@echo 'cBooterVersion        = "$(GhcVersion)"'                     >> $@
	@echo 'cStage                :: String'                             >> $@
	@echo 'cStage                = show (STAGE :: Int)'                 >> $@
	@echo 'cGccLinkerOpts        :: [String]'                           >> $@
	@echo 'cGccLinkerOpts        = words "$(CONF_GCC_LINKER_OPTS_STAGE$*)"' >> $@
	@echo 'cLdLinkerOpts         :: [String]'                           >> $@
	@echo 'cLdLinkerOpts         = words "$(CONF_LD_LINKER_OPTS_STAGE$*)"'  >> $@
	@echo 'cIntegerLibrary       :: String'                             >> $@
	@echo 'cIntegerLibrary       = "$(INTEGER_LIBRARY)"'                >> $@
	@echo 'cIntegerLibraryType   :: IntegerLibrary'                     >> $@
ifeq "$(INTEGER_LIBRARY)" "integer-gmp"
	@echo 'cIntegerLibraryType   = IntegerGMP'                          >> $@
else ifeq "$(INTEGER_LIBRARY)" "integer-simple"
	@echo 'cIntegerLibraryType   = IntegerSimple'                       >> $@
else ifneq "$(CLEANING)" "YES"
$(error Unknown integer library)
endif
	@echo 'cSupportsSplitObjs    :: String'                             >> $@
	@echo 'cSupportsSplitObjs    = "$(SupportsSplitObjs)"'              >> $@
	@echo 'cGhcWithInterpreter   :: String'                             >> $@
	@echo 'cGhcWithInterpreter   = "$(GhcWithInterpreter)"'             >> $@
	@echo 'cGhcWithNativeCodeGen :: String'                             >> $@
	@echo 'cGhcWithNativeCodeGen = "$(GhcWithNativeCodeGen)"'           >> $@
	@echo 'cGhcWithSMP           :: String'                             >> $@
	@echo 'cGhcWithSMP           = "$(GhcWithSMP)"'                     >> $@
	@echo 'cGhcRTSWays           :: String'                             >> $@
	@echo 'cGhcRTSWays           = "$(GhcRTSWays)"'                     >> $@
	@echo 'cGhcUnregisterised    :: String'                             >> $@
	@echo 'cGhcUnregisterised    = "$(GhcUnregisterised)"'              >> $@
	@echo 'cGhcEnableTablesNextToCode :: String'                        >> $@
	@echo 'cGhcEnableTablesNextToCode = "$(GhcEnableTablesNextToCode)"' >> $@
	@echo 'cLeadingUnderscore    :: String'                             >> $@
	@echo 'cLeadingUnderscore    = "$(LeadingUnderscore)"'              >> $@
	@echo 'cRAWCPP_FLAGS         :: String'                             >> $@
	@echo 'cRAWCPP_FLAGS         = "$(RAWCPP_FLAGS)"'                   >> $@
	@echo 'cLdHasNoCompactUnwind :: String'                             >> $@
	@echo 'cLdHasNoCompactUnwind = "$(LdHasNoCompactUnwind)"'           >> $@
	@echo 'cLdIsGNULd            :: String'                             >> $@
	@echo 'cLdIsGNULd            = "$(LdIsGNULd)"'                      >> $@
	@echo 'cLdHasBuildId         :: String'                             >> $@
	@echo 'cLdHasBuildId         = "$(LdHasBuildId)"'                   >> $@
	@echo 'cLD_X                 :: String'                             >> $@
	@echo 'cLD_X                 = "$(LD_X)"'                           >> $@
	@echo 'cGHC_DRIVER_DIR       :: String'                             >> $@
	@echo 'cGHC_DRIVER_DIR       = "$(GHC_DRIVER_DIR)"'                 >> $@
	@echo 'cGHC_UNLIT_PGM        :: String'                             >> $@
	@echo 'cGHC_UNLIT_PGM        = "$(GHC_UNLIT_PGM)"'                  >> $@
	@echo 'cGHC_UNLIT_DIR        :: String'                             >> $@
	@echo 'cGHC_UNLIT_DIR        = "$(GHC_UNLIT_DIR)"'                  >> $@
	@echo 'cGHC_SPLIT_PGM        :: String'                             >> $@
	@echo 'cGHC_SPLIT_PGM        = "$(GHC_SPLIT_PGM)"'                  >> $@
	@echo 'cGHC_SPLIT_DIR        :: String'                             >> $@
	@echo 'cGHC_SPLIT_DIR        = "$(GHC_SPLIT_DIR)"'                  >> $@
	@echo 'cGHC_SYSMAN_PGM       :: String'                             >> $@
	@echo 'cGHC_SYSMAN_PGM       = "$(GHC_SYSMAN)"'                     >> $@
	@echo 'cGHC_SYSMAN_DIR       :: String'                             >> $@
	@echo 'cGHC_SYSMAN_DIR       = "$(GHC_SYSMAN_DIR)"'                 >> $@
	@echo 'cDEFAULT_TMPDIR       :: String'                             >> $@
	@echo 'cDEFAULT_TMPDIR       = "$(DEFAULT_TMPDIR)"'                 >> $@
	@echo 'cLibFFI               :: Bool'                               >> $@
ifeq "$(UseLibFFIForAdjustors)" "YES"
	@echo 'cLibFFI               = True'                                >> $@
else
	@echo 'cLibFFI               = False'                               >> $@
endif
	@echo "cMPI_Opts             :: String"               >> $@
	@echo "cMPI_Opts             = \"$(MPIOpts)\""        >> $@
	@echo "cPVM_Root             :: String"               >> $@
	@echo "cPVM_Root             = \"$(PVM_ROOT)\""       >> $@
	@echo "cPVM_Arch             :: String"               >> $@
	@echo "cPVM_Arch             = \"$(PVM_ARCH)\""       >> $@
	@echo done.

# XXX 2010-08-19: This is a legacy clean. Remove later.
$(eval $(call clean-target,compiler,config_hs,compiler/main/Config.hs))

# -----------------------------------------------------------------------------
# Create platform includes

# Here we generate a little header file containing CPP symbols that GHC
# uses to determine which platform it is building on/for.  The platforms
# can differ between stage1 and stage2 if we're cross-compiling, so we
# need one of these header files per stage.

PLATFORM_H = ghc_boot_platform.h

compiler/stage1/$(PLATFORM_H) : mk/config.mk mk/project.mk | $$(dir $$@)/.
	$(call removeFiles,$@)
	@echo "Creating $@..."
	@echo "#ifndef __PLATFORM_H__"                           >> $@
	@echo "#define __PLATFORM_H__"                           >> $@
	@echo                                                    >> $@
	@echo "#define BuildPlatform_NAME  \"$(BUILDPLATFORM)\"" >> $@
	@echo "#define HostPlatform_NAME   \"$(BUILDPLATFORM)\"" >> $@
	@echo "#define TargetPlatform_NAME \"$(HOSTPLATFORM)\""  >> $@
	@echo                                                    >> $@
	@echo "#define $(BuildPlatform_CPP)_BUILD 1"             >> $@
	@echo "#define $(BuildPlatform_CPP)_HOST 1"              >> $@
	@echo "#define $(HostPlatform_CPP)_TARGET 1"             >> $@
	@echo                                                    >> $@
	@echo "#define $(BuildArch_CPP)_BUILD_ARCH 1"            >> $@
	@echo "#define $(BuildArch_CPP)_HOST_ARCH 1"             >> $@
	@echo "#define $(HostArch_CPP)_TARGET_ARCH 1"            >> $@
	@echo "#define BUILD_ARCH \"$(BuildArch_CPP)\""          >> $@
	@echo "#define HOST_ARCH \"$(BuildArch_CPP)\""           >> $@
	@echo "#define TARGET_ARCH \"$(HostArch_CPP)\""          >> $@
	@echo                                                    >> $@
	@echo "#define $(BuildOS_CPP)_BUILD_OS 1"                >> $@
	@echo "#define $(BuildOS_CPP)_HOST_OS 1"                 >> $@
	@echo "#define $(HostOS_CPP)_TARGET_OS 1"                >> $@
	@echo "#define BUILD_OS \"$(BuildOS_CPP)\""              >> $@
	@echo "#define HOST_OS \"$(BuildOS_CPP)\""               >> $@
	@echo "#define TARGET_OS \"$(HostOS_CPP)\""              >> $@
ifeq "$(HostOS_CPP)" "irix"
	@echo "#ifndef $(IRIX_MAJOR)_TARGET_OS"                  >> $@
	@echo "#define $(IRIX_MAJOR)_TARGET_OS 1"                >> $@
	@echo "#endif"                                           >> $@
endif
	@echo                                                    >> $@
	@echo "#define $(BuildVendor_CPP)_BUILD_VENDOR 1"        >> $@
	@echo "#define $(BuildVendor_CPP)_HOST_VENDOR 1"         >> $@
	@echo "#define $(HostVendor_CPP)_TARGET_VENDOR 1"        >> $@
	@echo "#define BUILD_VENDOR \"$(BuildVendor_CPP)\""      >> $@
	@echo "#define HOST_VENDOR \"$(BuildVendor_CPP)\""       >> $@
	@echo "#define TARGET_VENDOR \"$(HostVendor_CPP)\""      >> $@
	@echo                                                    >> $@
	@echo "#endif /* __PLATFORM_H__ */"                      >> $@
	@echo "Done."

# For stage2 and above, the BUILD platform is the HOST of stage1, and
# the HOST platform is the TARGET of stage1.  The TARGET remains the same
# (stage1 is the cross-compiler, not stage2).
compiler/stage2/$(PLATFORM_H) : mk/config.mk mk/project.mk | $$(dir $$@)/.
	$(call removeFiles,$@)
	@echo "Creating $@..."
	@echo "#ifndef __PLATFORM_H__"                            >> $@
	@echo "#define __PLATFORM_H__"                            >> $@
	@echo                                                     >> $@
	@echo "#define BuildPlatform_NAME  \"$(BUILDPLATFORM)\""  >> $@
	@echo "#define HostPlatform_NAME   \"$(HOSTPLATFORM)\""   >> $@
	@echo "#define TargetPlatform_NAME \"$(TARGETPLATFORM)\"" >> $@
	@echo                                                     >> $@
	@echo "#define $(BuildPlatform_CPP)_BUILD 1"              >> $@
	@echo "#define $(HostPlatform_CPP)_HOST 1"                >> $@
	@echo "#define $(TargetPlatform_CPP)_TARGET 1"            >> $@
	@echo                                                     >> $@
	@echo "#define $(BuildArch_CPP)_BUILD_ARCH 1"             >> $@
	@echo "#define $(HostArch_CPP)_HOST_ARCH 1"               >> $@
	@echo "#define $(TargetArch_CPP)_TARGET_ARCH 1"           >> $@
	@echo "#define BUILD_ARCH \"$(HostArch_CPP)\""            >> $@
	@echo "#define HOST_ARCH \"$(HostArch_CPP)\""             >> $@
	@echo "#define TARGET_ARCH \"$(TargetArch_CPP)\""         >> $@
	@echo                                                     >> $@
	@echo "#define $(HostOS_CPP)_BUILD_OS 1"                  >> $@
	@echo "#define $(HostOS_CPP)_HOST_OS 1"                   >> $@
	@echo "#define $(TargetOS_CPP)_TARGET_OS 1"               >> $@
	@echo "#define BUILD_OS \"$(HostOS_CPP)\""                >> $@
	@echo "#define HOST_OS \"$(HostOS_CPP)\""                 >> $@
	@echo "#define TARGET_OS \"$(TargetOS_CPP)\""             >> $@
ifeq "$(TargetOS_CPP)" "irix"
	@echo "#ifndef $(IRIX_MAJOR)_TARGET_OS"                   >> $@
	@echo "#define $(IRIX_MAJOR)_TARGET_OS 1"                 >> $@
	@echo "#endif"                                            >> $@
endif
	@echo                                                     >> $@
	@echo "#define $(BuildVendor_CPP)_BUILD_VENDOR 1"         >> $@
	@echo "#define $(HostVendor_CPP)_HOST_VENDOR 1"           >> $@
	@echo "#define $(TargetVendor_CPP)_TARGET_VENDOR  1"      >> $@
	@echo "#define BUILD_VENDOR \"$(BuildVendor_CPP)\""       >> $@
	@echo "#define HOST_VENDOR \"$(HostVendor_CPP)\""         >> $@
	@echo "#define TARGET_VENDOR \"$(TargetVendor_CPP)\""     >> $@
	@echo                                                     >> $@
	@echo "#endif /* __PLATFORM_H__ */"                       >> $@
	@echo "Done."

compiler/stage3/$(PLATFORM_H) : compiler/stage2/$(PLATFORM_H)
	"$(CP)" $< $@

# ----------------------------------------------------------------------------
#		Generate supporting stuff for prelude/PrimOp.lhs 
#		from prelude/primops.txt

# XXX: these should go in stage1/stage2/stage3
PRIMOP_BITS = compiler/primop-data-decl.hs-incl        \
              compiler/primop-tag.hs-incl              \
              compiler/primop-list.hs-incl             \
              compiler/primop-has-side-effects.hs-incl \
              compiler/primop-out-of-line.hs-incl      \
              compiler/primop-commutable.hs-incl       \
              compiler/primop-code-size.hs-incl        \
              compiler/primop-can-fail.hs-incl         \
              compiler/primop-strictness.hs-incl       \
              compiler/primop-primop-info.hs-incl

compiler_CPP_OPTS += -I$(GHC_INCLUDE_DIR)
compiler_CPP_OPTS += ${GhcCppOpts}

$(PRIMOPS_TXT) compiler/parser/Parser.y: %: %.pp compiler/stage1/$(PLATFORM_H)
	$(CPP) $(RAWCPP_FLAGS) -P $(compiler_CPP_OPTS) -x c $< | grep -v '^#pragma GCC' > $@

$(eval $(call clean-target,compiler,primop, $(PRIMOPS_TXT) compiler/parser/Parser.y $(PRIMOP_BITS)))

ifneq "$(BootingFromHc)" "YES"
compiler/primop-data-decl.hs-incl: $(PRIMOPS_TXT) $(GENPRIMOP_INPLACE)
	"$(GENPRIMOP_INPLACE)" --data-decl          < $< > $@
compiler/primop-tag.hs-incl: $(PRIMOPS_TXT) $(GENPRIMOP_INPLACE)
	"$(GENPRIMOP_INPLACE)" --primop-tag         < $< > $@
compiler/primop-list.hs-incl: $(PRIMOPS_TXT) $(GENPRIMOP_INPLACE)
	"$(GENPRIMOP_INPLACE)" --primop-list        < $< > $@
compiler/primop-has-side-effects.hs-incl: $(PRIMOPS_TXT) $(GENPRIMOP_INPLACE)
	"$(GENPRIMOP_INPLACE)" --has-side-effects   < $< > $@
compiler/primop-out-of-line.hs-incl: $(PRIMOPS_TXT) $(GENPRIMOP_INPLACE)
	"$(GENPRIMOP_INPLACE)" --out-of-line        < $< > $@
compiler/primop-commutable.hs-incl: $(PRIMOPS_TXT) $(GENPRIMOP_INPLACE)
	"$(GENPRIMOP_INPLACE)" --commutable         < $< > $@
compiler/primop-code-size.hs-incl: $(PRIMOPS_TXT) $(GENPRIMOP_INPLACE)
	"$(GENPRIMOP_INPLACE)" --code-size          < $< > $@
compiler/primop-can-fail.hs-incl: $(PRIMOPS_TXT) $(GENPRIMOP_INPLACE)
	"$(GENPRIMOP_INPLACE)" --can-fail           < $< > $@
compiler/primop-strictness.hs-incl: $(PRIMOPS_TXT) $(GENPRIMOP_INPLACE)
	"$(GENPRIMOP_INPLACE)" --strictness         < $< > $@
compiler/primop-primop-info.hs-incl: $(PRIMOPS_TXT) $(GENPRIMOP_INPLACE)
	"$(GENPRIMOP_INPLACE)" --primop-primop-info < $< > $@

# Usages aren't used any more; but the generator 
# can still generate them if we want them back
compiler/primop-usage.hs-incl: $(PRIMOPS_TXT)
	"$(GENPRIMOP_INPLACE)" --usage              < $< > $@
endif

# -----------------------------------------------------------------------------
# Configuration

compiler_stage1_CONFIGURE_OPTS += --flags=stage1
compiler_stage2_CONFIGURE_OPTS += --flags=stage2
compiler_stage3_CONFIGURE_OPTS += --flags=stage3

ifeq "$(GhcWithNativeCodeGen)" "YES"
compiler_stage1_CONFIGURE_OPTS += --flags=ncg
compiler_stage2_CONFIGURE_OPTS += --flags=ncg
endif

ifeq "$(GhcWithInterpreter)" "YES"
compiler_stage2_CONFIGURE_OPTS += --flags=ghci

ifeq "$(BuildSharedLibs)" "YES"
compiler_stage2_CONFIGURE_OPTS += --enable-shared
# If we are going to use dynamic libraries instead of .o files for ghci,
# we will need to always retain CAFs in the compiler.
# ghci/keepCAFsForGHCi contains a GNU C __attribute__((constructor))
# function which sets the keepCAFs flag for the RTS before any Haskell
# code is run.
compiler_stage2_CONFIGURE_OPTS += --flags=dynlibs
endif

ifeq "$(GhcEnableTablesNextToCode) $(GhcUnregisterised)" "YES NO"
# Should GHCI be building info tables in the TABLES_NEXT_TO_CODE style
# or not?
# XXX This should logically be a CPP option, but there doesn't seem to
# be a flag for that
compiler_stage2_CONFIGURE_OPTS += --ghc-option=-DGHCI_TABLES_NEXT_TO_CODE
endif

# Should the debugger commands be enabled?
ifeq "$(GhciWithDebugger)" "YES"
compiler_stage2_CONFIGURE_OPTS += --ghc-option=-DDEBUGGER
endif

endif

ifeq "$(TargetOS_CPP)" "openbsd"
compiler_CONFIGURE_OPTS += --ld-options=-E
endif

ifeq "$(GhcUnregisterised)" "NO"
else
compiler_CONFIGURE_OPTS += --ghc-option=-DNO_REGS
endif

ifeq "$(GhcProfiled)" "YES"

# If we're profiling GHC then we want SCCs.  However, adding -auto-all
# everywhere tends to give a hard-to-read profile, and adds lots of
# overhead.  A better approach is to proceed top-down; identify the
# parts of the compiler of interest, and then add further cost centres
# as necessary.  Turn on -auto-all for individual modules like this:

compiler/main/DriverPipeline_HC_OPTS += -auto-all
compiler/main/GhcMake_HC_OPTS        += -auto-all
compiler/main/GHC_HC_OPTS            += -auto-all

# or alternatively addd {-# OPTIONS_GHC -auto-all #-} to the top of
# modules you're interested in.

# We seem to still build the vanilla libraries even if we say
# --disable-library-vanilla, but installation then fails, as Cabal
# doesn't copy the vanilla .hi files, but ghc-pkg complains about
# their absence when we register the package. So for now, we just
# leave the vanilla libraries enabled.
# compiler_stage2_CONFIGURE_OPTS += --disable-library-vanilla
compiler_stage2_CONFIGURE_OPTS += --ghc-pkg-option=--force
endif

compiler_stage3_CONFIGURE_OPTS := $(compiler_stage2_CONFIGURE_OPTS)

compiler_stage1_CONFIGURE_OPTS += --ghc-option=-DSTAGE=1
compiler_stage2_CONFIGURE_OPTS += --ghc-option=-DSTAGE=2
compiler_stage3_CONFIGURE_OPTS += --ghc-option=-DSTAGE=3
compiler_stage2_HADDOCK_OPTS += --optghc=-DSTAGE=2

compiler/stage1/package-data.mk : compiler/ghc.mk
compiler/stage2/package-data.mk : compiler/ghc.mk
compiler/stage3/package-data.mk : compiler/ghc.mk

# -----------------------------------------------------------------------------
# And build the package

compiler_PACKAGE = ghc

# Note [fiddle-stage1-version]
# The version of the GHC package changes every day, since the
# patchlevel is the current date.  We don't want to force
# recompilation of the entire compiler when this happens, so for stage
# 1 we omit the patchlevel from the version number.  For stage 2 we
# have to include the patchlevel since this is the package we install,
# however.
#
# Note: we also have to tweak the version number of the package itself
# when it gets registered; see Note [munge-stage1-package-config]
# below.
# The ProjectPatchLevel > 20000000 iff it's a date. If it's e.g. 6.12.1
# then we don't want to remove it
ifneq "$(CLEANING)" "YES"
ifeq "$(shell [ $(ProjectPatchLevel) -gt 20000000 ] && echo YES)" "YES"
compiler_stage1_VERSION_MUNGED = YES
endif
endif

ifeq "$(compiler_stage1_VERSION_MUNGED)" "YES"
define compiler_PACKAGE_MAGIC
compiler_stage1_VERSION = $(subst .$(ProjectPatchLevel),,$(ProjectVersion))
endef

# Don't register the non-munged package
compiler_stage1_REGISTER_PACKAGE = NO

endif

# haddocking only happens for stage2
compiler_stage1_DO_HADDOCK = NO
compiler_stage3_DO_HADDOCK = NO

# Don't do splitting for the GHC package, it takes too long and
# there's not much benefit.
compiler_stage1_SplitObjs = NO
compiler_stage2_SplitObjs = NO
compiler_stage3_SplitObjs = NO

# if stage is set to something other than "1" or "", disable stage 1
ifneq "$(filter-out 1,$(stage))" ""
compiler_stage1_NOT_NEEDED = YES
endif
# if stage is set to something other than "2" or "", disable stage 2
ifneq "$(filter-out 2,$(stage))" ""
compiler_stage2_NOT_NEEDED = YES
endif
# stage 3 has to be requested explicitly with stage=3
ifneq "$(stage)" "3"
compiler_stage3_NOT_NEEDED = YES
endif
$(eval $(call build-package,compiler,stage1,0))
$(eval $(call build-package,compiler,stage2,1))
$(eval $(call build-package,compiler,stage3,2))

# after build-package, because that adds --enable-library-for-ghci
# to compiler_stage*_CONFIGURE_OPTS:
# We don't build the GHCi library for the ghc package. We can load it
# the .a file instead, and as object splitting isn't on for the ghc
# package this isn't much slower.However, not building the package saves
# a significant chunk of disk space.
compiler_stage1_CONFIGURE_OPTS += --disable-library-for-ghci
compiler_stage2_CONFIGURE_OPTS += --disable-library-for-ghci
compiler_stage3_CONFIGURE_OPTS += --disable-library-for-ghci

# after build-package, because that sets compiler_stage1_HC_OPTS:
compiler_stage1_HC_OPTS += $(GhcStage1HcOpts)
compiler_stage2_HC_OPTS += $(GhcStage2HcOpts)
compiler_stage3_HC_OPTS += $(GhcStage3HcOpts)

ifeq "$(GhcStage1DefaultNewCodegen)" "YES"
compiler_stage1_HC_OPTS += -DGHC_DEFAULT_NEW_CODEGEN
endif

ifeq "$(GhcStage2DefaultNewCodegen)" "YES"
compiler_stage2_HC_OPTS += -DGHC_DEFAULT_NEW_CODEGEN
endif

ifeq "$(GhcStage3DefaultNewCodegen)" "YES"
compiler_stage3_HC_OPTS += -DGHC_DEFAULT_NEW_CODEGEN
endif

ifneq "$(BINDIST)" "YES"

compiler_stage2_TAGS_HC_OPTS = -package ghc
$(eval $(call tags-package,compiler,stage2))

$(compiler_stage1_depfile_haskell) : compiler/stage1/$(PLATFORM_H)
$(compiler_stage2_depfile_haskell) : compiler/stage2/$(PLATFORM_H)
$(compiler_stage3_depfile_haskell) : compiler/stage3/$(PLATFORM_H)

$(compiler_stage1_depfile_haskell) : $(includes_H_CONFIG) $(includes_H_PLATFORM) $(includes_GHCCONSTANTS) $(includes_DERIVEDCONSTANTS) $(PRIMOP_BITS)
$(compiler_stage2_depfile_haskell) : $(includes_H_CONFIG) $(includes_H_PLATFORM) $(includes_GHCCONSTANTS) $(includes_DERIVEDCONSTANTS) $(PRIMOP_BITS)
$(compiler_stage3_depfile_haskell) : $(includes_H_CONFIG) $(includes_H_PLATFORM) $(includes_GHCCONSTANTS) $(includes_DERIVEDCONSTANTS) $(PRIMOP_BITS)

# Every Constants.o object file depends on includes/GHCConstants.h:
$(eval $(call compiler-hs-dependency,Constants,$(includes_GHCCONSTANTS) includes/HaskellConstants.hs))

# Every PrimOp.o object file depends on $(PRIMOP_BITS):
$(eval $(call compiler-hs-dependency,PrimOp,$(PRIMOP_BITS)))

# GHC itself doesn't know about the above dependencies, so we have to
# switch off the recompilation checker for those modules:
compiler/prelude/PrimOp_HC_OPTS  += -fforce-recomp
compiler/main/Constants_HC_OPTS  += -fforce-recomp

# Workaround for #4003 in GHC 6.12.2.  It didn't happen in 6.12.1, and
# will be fixed in 6.12.3.  Unfortunately we don't have a way to do
# this for just stage1 in the build system.
ifeq "$(GhcVersion)" "6.12.2"
compiler/hsSyn/HsLit_HC_OPTS     += -fomit-interface-pragmas
endif

# LibFFI.hs #includes ffi.h
compiler/stage2/build/LibFFI.hs : $(libffi_HEADERS)
# On Windows it seems we also need to link directly to libffi
ifeq  "$(HOSTPLATFORM)" "i386-unknown-mingw32"
define windowsDynLinkToFfi
# $1 = way
ifneq "$$(findstring dyn, $1)" ""
compiler_stage2_$1_ALL_HC_OPTS += -lffi-5
endif
endef
$(foreach way,$(GhcLibWays),$(eval $(call windowsDynLinkToFfi,$(way))))
endif

# Note [munge-stage1-package-config]
# Strip the date/patchlevel from the version of stage1.  See Note
# [fiddle-stage1-version] above.
ifeq "$(compiler_stage1_VERSION_MUNGED)" "YES"
compiler/stage1/inplace-pkg-config-munged: compiler/stage1/inplace-pkg-config
	sed -e 's/^\(version: .*\)\.$(ProjectPatchLevel)$$/\1/' \
	    -e 's/^\(id: .*\)\.$(ProjectPatchLevel)$$/\1/' \
	    -e 's/^\(hs-libraries: HSghc-.*\)\.$(ProjectPatchLevel)$$/\1/' \
	  < $< > $@
	"$(compiler_stage1_GHC_PKG)" update --force $(compiler_stage1_GHC_PKG_OPTS) $@

# We need to make sure the munged config is in the database before we
# try to configure ghc-bin
ghc/stage1/package-data.mk : compiler/stage1/inplace-pkg-config-munged
endif

endif


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


define build-package-way # $1 = dir, $2 = distdir, $3 = way, $4 = stage
$(call trace, build-package-way($1,$2,$3))
$(call profStart, build-package-way($1,$2,$3))

$(call distdir-way-opts,$1,$2,$3,$4)
$(call hs-suffix-rules,$1,$2,$3)
$$(foreach dir,$$($1_$2_HS_SRC_DIRS),\
  $$(eval $$(call hs-suffix-rules-srcdir,$1,$2,$3,$$(dir))))

$(call hs-objs,$1,$2,$3)

# The .a/.so library file, indexed by two different sets of vars:
# the first is indexed by the dir, distdir and way
# the second is indexed by the package id, distdir and way
$1_$2_$3_LIB = $1/$2/build/libHS$$($1_PACKAGE)-$$($1_$2_VERSION)$$($3_libsuf)
$$($1_PACKAGE)-$($1_$2_VERSION)_$2_$3_LIB = $$($1_$2_$3_LIB)

# hack: the DEPS_LIBS mechanism assumes that the distdirs for packges
# that depend on each other are the same, but that is not the case for
# ghc where we use stage1/stage2 rather than dist/dist-install.
# Really we should use a consistent scheme for distdirs, but in the
# meantime we work around it by defining ghc-<ver>_dist-install_way_LIB:
ifeq "$$($1_PACKAGE) $2" "ghc stage2"
$$($1_PACKAGE)-$($1_$2_VERSION)_dist-install_$3_LIB = $$($1_$2_$3_LIB)
endif

# All the .a/.so library file dependencies for this library
$1_$2_$3_DEPS_LIBS=$$(foreach dep,$$($1_$2_DEPS),$$($$(dep)_$2_$3_LIB))

ifeq "$$(BootingFromHc)" "YES"
$1_$2_$3_C_OBJS += $$(shell $$(FIND) $1/$2/build -name "*_stub.c" -print | sed 's/c$$$$/o/')
endif

$1_$2_$3_NON_HS_OBJS = $$($1_$2_$3_CMM_OBJS) $$($1_$2_$3_C_OBJS)  $$($1_$2_$3_S_OBJS) $$($1_$2_EXTRA_OBJS)
$1_$2_$3_ALL_OBJS = $$($1_$2_$3_HS_OBJS) $$($1_$2_$3_NON_HS_OBJS)

# The quadrupled $'s here are because the _v_LIB variables aren't
# necessarily set when this part of the makefile is read.
# These deps aren't technically necessary in themselves, but they
# turn the dependencies of programs on libraries into transitive
# dependencies.
ifeq "$4" "0"
$$($1_$2_$3_LIB) : $$(foreach dep,$$($1_$2_DEP_NAMES),$$$$(libraries/$$(dep)_dist-boot_v_LIB))
else
$$($1_$2_$3_LIB) : $$(foreach dep,$$($1_$2_DEP_NAMES),$$$$(libraries/$$(dep)_dist-install_v_LIB))
endif

ifeq "$3" "dyn"

# Link a dynamic library
# On windows we have to supply the extra libs this one links to when building it.
ifeq "$$(HOSTPLATFORM)" "i386-unknown-mingw32"
$$($1_$2_$3_LIB) : $$($1_$2_$3_ALL_OBJS) $$(ALL_RTS_LIBS) $$($1_$2_$3_DEPS_LIBS)
	"$$($1_$2_HC)" $$($1_$2_$3_ALL_HC_OPTS) $$($1_$2_$3_ALL_OBJS) \
         -shared -dynamic -dynload deploy \
	 $$(addprefix -l,$$($1_$2_EXTRA_LIBRARIES)) \
         -no-auto-link-packages \
         -o $$@
else
$$($1_$2_$3_LIB) : $$($1_$2_$3_ALL_OBJS) $$(ALL_RTS_LIBS) $$($1_$2_$3_DEPS_LIBS)
	"$$($1_$2_HC)" $$($1_$2_$3_ALL_HC_OPTS) $$($1_$2_$3_ALL_OBJS) \
         -shared -dynamic -dynload deploy \
	     -dylib-install-name $(ghclibdir)/`basename "$$@" | sed 's/^libHS//;s/[-]ghc.*//'`/`basename "$$@"` \
         -no-auto-link-packages \
         -o $$@
endif
else
# Build the ordinary .a library
$$($1_$2_$3_LIB) : $$($1_$2_$3_ALL_OBJS)
	$$(call removeFiles,$$@ $$@.contents)
ifeq "$$($1_$2_SplitObjs)" "YES"
	$$(FIND) $$(patsubst %.$$($3_osuf),%_$$($3_osuf)_split,$$($1_$2_$3_HS_OBJS)) -name '*.$$($3_osuf)' -print >> $$@.contents
	echo $$($1_$2_$3_NON_HS_OBJS) >> $$@.contents
else
	echo $$($1_$2_$3_ALL_OBJS) >> $$@.contents
endif
ifeq "$$($1_$2_ArSupportsAtFile)" "YES"
	"$$($1_$2_AR)" $$($1_$2_AR_OPTS) $$($1_$2_EXTRA_AR_ARGS) $$@ @$$@.contents
else
	"$$(XARGS)" $$(XARGS_OPTS) "$$($1_$2_AR)" $$($1_$2_AR_OPTS) $$($1_$2_EXTRA_AR_ARGS) $$@ < $$@.contents
endif
	$$(call removeFiles,$$@.contents)
endif

$(call all-target,$1_$2,all_$1_$2_$3)
$(call all-target,$1_$2_$3,$$($1_$2_$3_LIB))

# Don't put bootstrapping packages in the bindist
ifneq "$4" "0"
BINDIST_HI += $$($1_$2_$3_HI)
BINDIST_LIBS += $$($1_$2_$3_LIB)
endif

# Build the GHCi library
ifeq "$3" "v"
$1_$2_GHCI_LIB = $1/$2/build/HS$$($1_PACKAGE)-$$($1_$2_VERSION).$$($3_osuf)
ifeq "$$($1_$2_BUILD_GHCI_LIB)" "YES"
# Don't put bootstrapping packages in the bindist
ifneq "$4" "0"
BINDIST_LIBS += $$($1_$2_GHCI_LIB)
endif
endif
$$($1_$2_GHCI_LIB) : $$($1_$2_$3_HS_OBJS) $$($1_$2_$3_CMM_OBJS) $$($1_$2_$3_C_OBJS) $$($1_$2_$3_S_OBJS) $$($1_$2_EXTRA_OBJS)
	"$$(LD)" $$(CONF_LD_LINKER_OPTS_STAGE$4) -r -o $$@ $$(EXTRA_LD_OPTS) $$($1_$2_$3_HS_OBJS) $$($1_$2_$3_CMM_OBJS) $$($1_$2_$3_C_OBJS) $$($1_$2_$3_S_OBJS) $$($1_$2_EXTRA_OBJS)

ifeq "$$($1_$2_BUILD_GHCI_LIB)" "YES"
# Don't bother making ghci libs for bootstrapping packages
ifneq "$4" "0"
$(call all-target,$1_$2,$$($1_$2_GHCI_LIB))
endif
endif
endif

$(call profEnd, build-package-way($1,$2,$3))
endef


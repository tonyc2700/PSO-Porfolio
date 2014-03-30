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

utils/genprimopcode_dist_MODULES = Lexer Main ParserM Parser Syntax
utils/genprimopcode_dist_PROG    = $(GHC_GENPRIMOP_PGM)
utils/genprimopcode_dist_HC_OPTS = -package array

$(eval $(call build-prog,utils/genprimopcode,dist,0))

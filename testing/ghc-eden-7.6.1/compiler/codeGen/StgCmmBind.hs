-----------------------------------------------------------------------------
--
-- Stg to C-- code generation: bindings
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module StgCmmBind (
	cgTopRhsClosure,
	cgBind,
	emitBlackHoleCode,
        pushUpdateFrame
  ) where

#include "HsVersions.h"

import StgCmmExpr
import StgCmmMonad
import StgCmmEnv
import StgCmmCon
import StgCmmHeap
import StgCmmProf
import StgCmmTicky
import StgCmmGran
import StgCmmLayout
import StgCmmUtils
import StgCmmClosure
import StgCmmForeign    (emitPrimCall)

import MkGraph
import CoreSyn		( AltCon(..) )
import SMRep
import Cmm
import CmmUtils
import CLabel
import StgSyn
import CostCentre
import Id
import Control.Monad
import Name
import Module
import ListSetOps
import Util
import BasicTypes
import Constants
import Outputable
import FastString
import Maybes
import DynFlags
import StaticFlags

------------------------------------------------------------------------
--		Top-level bindings
------------------------------------------------------------------------

-- For closures bound at top level, allocate in static space.
-- They should have no free variables.

cgTopRhsClosure :: Id
		-> CostCentreStack	-- Optional cost centre annotation
		-> StgBinderInfo
		-> UpdateFlag
                -> [Id]                 -- Args
		-> StgExpr
		-> FCode CgIdInfo

cgTopRhsClosure id ccs _ upd_flag args body = do
  {	-- LAY OUT THE OBJECT
    let name = idName id
  ; lf_info <- mkClosureLFInfo id TopLevel [] upd_flag args
  ; mod_name <- getModuleName
  ; dflags   <- getDynFlags
  ; let descr         = closureDescription dflags mod_name name
        closure_info  = mkClosureInfo True id lf_info 0 0 descr
        closure_label = mkLocalClosureLabel name (idCafInfo id)
    	cg_id_info    = litIdInfo id lf_info (CmmLabel closure_label)
        caffy         = idCafInfo id
        info_tbl      = mkCmmInfo closure_info -- XXX short-cut
        closure_rep   = mkStaticClosureFields info_tbl ccs caffy []

  	 -- BUILD THE OBJECT, AND GENERATE INFO TABLE (IF NECESSARY)
  ; emitDataLits closure_label closure_rep
  ; let fv_details :: [(NonVoid Id, VirtualHpOffset)]
	(_, _, fv_details) = mkVirtHeapOffsets (isLFThunk lf_info)
				               (addIdReps [])
  -- Don't drop the non-void args until the closure info has been made
  ; forkClosureBody (closureCodeBody True id closure_info ccs
                                     (nonVoidIds args) (length args) body fv_details)

  ; returnFC cg_id_info }

------------------------------------------------------------------------
--		Non-top-level bindings
------------------------------------------------------------------------

cgBind :: StgBinding -> FCode ()
cgBind (StgNonRec name rhs)
  = do	{ ((info, init), body) <- getCodeR $ cgRhs name rhs
        ; addBindC (cg_id info) info
        ; emit (init <*> body) }

cgBind (StgRec pairs)
  = do  { ((new_binds, inits), body) <- getCodeR $ fixC (\ new_binds_inits ->
               do { addBindsC $ fst new_binds_inits -- avoid premature deconstruction
                  ; liftM unzip $ listFCs [ cgRhs b e | (b,e) <- pairs ] })
       ; addBindsC new_binds
       ; emit (catAGraphs inits <*> body) }

{- Recursive let-bindings are tricky.
   Consider the following pseudocode:
     let x = \_ ->  ... y ...
         y = \_ ->  ... z ...
         z = \_ ->  ... x ...
     in ...
   For each binding, we need to allocate a closure, and each closure must
   capture the address of the other closures.
   We want to generate the following C-- code:
     // Initialization Code
     x = hp - 24; // heap address of x's closure
     y = hp - 40; // heap address of x's closure
     z = hp - 64; // heap address of x's closure
     // allocate and initialize x
     m[hp-8]   = ...
     m[hp-16]  = y       // the closure for x captures y
     m[hp-24] = x_info;
     // allocate and initialize y
     m[hp-32] = z;       // the closure for y captures z
     m[hp-40] = y_info;
     // allocate and initialize z
     ...

   For each closure, we must generate not only the code to allocate and
   initialize the closure itself, but also some Initialization Code that
   sets a variable holding the closure pointer.
   The complication here is that we don't know the heap offsets a priori,
   which has two consequences:
     1. we need a fixpoint
     2. we can't trivially separate the Initialization Code from the
        code that compiles the right-hand-sides

   Note: We don't need this complication with let-no-escapes, because
   in that case, the names are bound to labels in the environment,
   and we don't need to emit any code to witness that binding.
-}

--------------------
cgRhs :: Id -> StgRhs -> FCode (CgIdInfo, CmmAGraph)
   -- The Id is passed along so a binding can be set up
   -- The returned values are the binding for the environment
   -- and the Initialization Code that witnesses the binding

cgRhs name (StgRhsCon cc con args)
  = buildDynCon name cc con args

cgRhs name (StgRhsClosure cc bi fvs upd_flag _srt args body)
  = mkRhsClosure name cc bi (nonVoidIds fvs) upd_flag args body

------------------------------------------------------------------------
--		Non-constructor right hand sides
------------------------------------------------------------------------

mkRhsClosure :: Id -> CostCentreStack -> StgBinderInfo
	     -> [NonVoid Id]			-- Free vars
             -> UpdateFlag
	     -> [Id]			        -- Args
	     -> StgExpr
	     -> FCode (CgIdInfo, CmmAGraph)

{- mkRhsClosure looks for two special forms of the right-hand side:
	a) selector thunks
	b) AP thunks

If neither happens, it just calls mkClosureLFInfo.  You might think
that mkClosureLFInfo should do all this, but it seems wrong for the
latter to look at the structure of an expression

Note [Selectors]
~~~~~~~~~~~~~~~~
We look at the body of the closure to see if it's a selector---turgid,
but nothing deep.  We are looking for a closure of {\em exactly} the
form:

...  = [the_fv] \ u [] ->
	 case the_fv of
	   con a_1 ... a_n -> a_i

Note [Ap thunks]
~~~~~~~~~~~~~~~~
A more generic AP thunk of the form

	x = [ x_1...x_n ] \.. [] -> x_1 ... x_n

A set of these is compiled statically into the RTS, so we just use
those.  We could extend the idea to thunks where some of the x_i are
global ids (and hence not free variables), but this would entail
generating a larger thunk.  It might be an option for non-optimising
compilation, though.

We only generate an Ap thunk if all the free variables are pointers,
for semi-obvious reasons.

-}

---------- Note [Selectors] ------------------
mkRhsClosure	bndr cc bi
		[NonVoid the_fv]   		-- Just one free var
		upd_flag		-- Updatable thunk
                []                      -- A thunk
		body@(StgCase (StgApp scrutinee [{-no args-}])
		      _ _ _ _   -- ignore uniq, etc.
		      (AlgAlt _)
		      [(DataAlt _, params, _use_mask,
			    (StgApp selectee [{-no args-}]))])
  |  the_fv == scrutinee		-- Scrutinee is the only free variable
  && maybeToBool maybe_offset		-- Selectee is a component of the tuple
  && offset_into_int <= mAX_SPEC_SELECTEE_SIZE	-- Offset is small enough
  = -- NOT TRUE: ASSERT(is_single_constructor)
    -- The simplifier may have statically determined that the single alternative
    -- is the only possible case and eliminated the others, even if there are
    -- other constructors in the datatype.  It's still ok to make a selector
    -- thunk in this case, because we *know* which constructor the scrutinee
    -- will evaluate to.
    --
    -- srt is discarded; it must be empty
    cgStdThunk bndr cc bi body lf_info [StgVarArg the_fv]
  where
    lf_info 		  = mkSelectorLFInfo bndr offset_into_int
				 (isUpdatable upd_flag)
    (_, _, params_w_offsets) = mkVirtConstrOffsets (addIdReps params)
			       -- Just want the layout
    maybe_offset	  = assocMaybe params_w_offsets (NonVoid selectee)
    Just the_offset 	  = maybe_offset
    offset_into_int       = the_offset - fixedHdrSize

---------- Note [Ap thunks] ------------------
mkRhsClosure    bndr cc bi
		fvs
		upd_flag
                []                      -- No args; a thunk
		body@(StgApp fun_id args)

  | args `lengthIs` (arity-1)
 	&& all (isGcPtrRep . idPrimRep . stripNV) fvs
 	&& isUpdatable upd_flag
 	&& arity <= mAX_SPEC_AP_SIZE
        && not opt_SccProfilingOn -- not when profiling: we don't want to
                                  -- lose information about this particular
                                  -- thunk (e.g. its type) (#949)

                   -- Ha! an Ap thunk
  = cgStdThunk bndr cc bi body lf_info payload
  where
	lf_info = mkApLFInfo bndr upd_flag arity
	-- the payload has to be in the correct order, hence we can't
 	-- just use the fvs.
	payload = StgVarArg fun_id : args
	arity 	= length fvs

---------- Default case ------------------
mkRhsClosure bndr cc _ fvs upd_flag args body
  = do	{ 	-- LAY OUT THE OBJECT
	-- If the binder is itself a free variable, then don't store
	-- it in the closure.  Instead, just bind it to Node on entry.
	-- NB we can be sure that Node will point to it, because we
	-- haven't told mkClosureLFInfo about this; so if the binder
	-- _was_ a free var of its RHS, mkClosureLFInfo thinks it *is*
	-- stored in the closure itself, so it will make sure that
	-- Node points to it...
	; let
		is_elem	     = isIn "cgRhsClosure"
		bndr_is_a_fv = (NonVoid bndr) `is_elem` fvs
		reduced_fvs | bndr_is_a_fv = fvs `minusList` [NonVoid bndr]
			    | otherwise	   = fvs


	-- MAKE CLOSURE INFO FOR THIS CLOSURE
	; lf_info <- mkClosureLFInfo bndr NotTopLevel fvs upd_flag args
	; mod_name <- getModuleName
        ; dflags <- getDynFlags
        ; let   name  = idName bndr
                descr = closureDescription dflags mod_name name
                fv_details :: [(NonVoid Id, VirtualHpOffset)]
		(tot_wds, ptr_wds, fv_details)
		   = mkVirtHeapOffsets (isLFThunk lf_info)
				       (addIdReps (map stripNV reduced_fvs))
		closure_info = mkClosureInfo False	-- Not static
					     bndr lf_info tot_wds ptr_wds
                                             descr

	-- BUILD ITS INFO TABLE AND CODE
	; forkClosureBody $
		-- forkClosureBody: (a) ensure that bindings in here are not seen elsewhere
		-- 		    (b) ignore Sequel from context; use empty Sequel
		-- And compile the body
		closureCodeBody False bndr closure_info cc (nonVoidIds args)
                                (length args) body fv_details

	-- BUILD THE OBJECT
--      ; (use_cc, blame_cc) <- chooseDynCostCentres cc args body
        ; let use_cc = curCCS; blame_cc = curCCS
        ; emit (mkComment $ mkFastString "calling allocDynClosure")
        ; let toVarArg (NonVoid a, off) = (NonVoid (StgVarArg a), off)
        ; let info_tbl = mkCmmInfo closure_info
        ; (tmp, init) <- allocDynClosure info_tbl lf_info use_cc blame_cc
                                         (map toVarArg fv_details)

	-- RETURN
	; regIdInfo bndr lf_info tmp init }

-- Use with care; if used inappropriately, it could break invariants.
stripNV :: NonVoid a -> a
stripNV (NonVoid a) = a

-------------------------
cgStdThunk
	:: Id
	-> CostCentreStack	-- Optional cost centre annotation
	-> StgBinderInfo	-- XXX: not used??
	-> StgExpr
	-> LambdaFormInfo
	-> [StgArg]			-- payload
	-> FCode (CgIdInfo, CmmAGraph)

cgStdThunk bndr _cc _bndr_info _body lf_info payload
  = do	-- AHA!  A STANDARD-FORM THUNK
  {	-- LAY OUT THE OBJECT
    mod_name <- getModuleName
  ; dflags <- getDynFlags
  ; let (tot_wds, ptr_wds, payload_w_offsets)
	    = mkVirtHeapOffsets (isLFThunk lf_info) (addArgReps payload)

	descr = closureDescription dflags mod_name (idName bndr)
	closure_info = mkClosureInfo False 	-- Not static
				     bndr lf_info tot_wds ptr_wds
                                     descr

--  ; (use_cc, blame_cc) <- chooseDynCostCentres cc [{- no args-}] body
  ; let use_cc = curCCS; blame_cc = curCCS

	-- BUILD THE OBJECT
  ; let info_tbl = mkCmmInfo closure_info
  ; (tmp, init) <- allocDynClosure info_tbl lf_info
                                   use_cc blame_cc payload_w_offsets

	-- RETURN
  ; regIdInfo bndr lf_info tmp init }

mkClosureLFInfo :: Id		-- The binder
		-> TopLevelFlag	-- True of top level
		-> [NonVoid Id]	-- Free vars
		-> UpdateFlag 	-- Update flag
		-> [Id]         -- Args
		-> FCode LambdaFormInfo
mkClosureLFInfo bndr top fvs upd_flag args
  | null args = return (mkLFThunk (idType bndr) top (map stripNV fvs) upd_flag)
  | otherwise = do { arg_descr <- mkArgDescr (idName bndr) args
		   ; return (mkLFReEntrant top (map stripNV fvs) args arg_descr) }


------------------------------------------------------------------------
--		The code for closures}
------------------------------------------------------------------------

closureCodeBody :: Bool            -- whether this is a top-level binding
                -> Id              -- the closure's name
		-> ClosureInfo	   -- Lots of information about this closure
		-> CostCentreStack -- Optional cost centre attached to closure
	 	-> [NonVoid Id]    -- incoming args to the closure
	 	-> Int             -- arity, including void args
		-> StgExpr
		-> [(NonVoid Id, VirtualHpOffset)] -- the closure's free vars
		-> FCode ()

{- There are two main cases for the code for closures.

* If there are *no arguments*, then the closure is a thunk, and not in
  normal form. So it should set up an update frame (if it is
  shared). NB: Thunks cannot have a primitive type!

* If there is *at least one* argument, then this closure is in
  normal form, so there is no need to set up an update frame.

  The Macros for GrAnSim are produced at the beginning of the
  argSatisfactionCheck (by calling fetchAndReschedule).
  There info if Node points to closure is available. -- HWL -}

closureCodeBody top_lvl bndr cl_info cc args arity body fv_details
  | length args == 0 -- No args i.e. thunk
  = emitClosureProcAndInfoTable top_lvl bndr lf_info info_tbl [] $
      \(_, node, _) -> thunkCode cl_info fv_details cc node arity body
   where
     lf_info  = closureLFInfo cl_info
     info_tbl = mkCmmInfo cl_info

closureCodeBody top_lvl bndr cl_info _cc args arity body fv_details
  = ASSERT( length args > 0 )
    do  { -- Allocate the global ticky counter,
          -- and establish the ticky-counter
          -- label for this block
          let ticky_ctr_lbl = closureRednCountsLabel cl_info
        ; emitTickyCounter cl_info (map stripNV args)
        ; setTickyCtrLabel ticky_ctr_lbl $ do

        ; let
             lf_info  = closureLFInfo cl_info
             info_tbl = mkCmmInfo cl_info

        -- Emit the main entry code
        ; emitClosureProcAndInfoTable top_lvl bndr lf_info info_tbl args $
            \(offset, node, arg_regs) -> do
                -- Emit slow-entry code (for entering a closure through a PAP)
                { mkSlowEntryCode cl_info arg_regs

                ; let lf_info = closureLFInfo cl_info
                      node_points = nodeMustPointToIt lf_info
                      node' = if node_points then Just node else Nothing
                ; tickyEnterFun cl_info
                ; whenC node_points (ldvEnterClosure cl_info)
                ; granYield arg_regs node_points

                -- Main payload
                ; entryHeapCheck cl_info offset node' arity arg_regs $ do
                { fv_bindings <- mapM bind_fv fv_details
                -- Load free vars out of closure *after*
                -- heap check, to reduce live vars over check
                ; if node_points then load_fvs node lf_info fv_bindings
                                 else return ()
                ; cgExpr body }}
  }

-- A function closure pointer may be tagged, so we
-- must take it into account when accessing the free variables.
bind_fv :: (NonVoid Id, VirtualHpOffset) -> FCode (LocalReg, WordOff)
bind_fv (id, off) = do { reg <- rebindToReg id; return (reg, off) }

load_fvs :: LocalReg -> LambdaFormInfo -> [(LocalReg, WordOff)] -> FCode ()
load_fvs node lf_info = mapCs (\ (reg, off) ->
      emit $ mkTaggedObjectLoad reg node off tag)
  where tag = lfDynTag lf_info

-----------------------------------------
-- The "slow entry" code for a function.  This entry point takes its
-- arguments on the stack.  It loads the arguments into registers
-- according to the calling convention, and jumps to the function's
-- normal entry point.  The function's closure is assumed to be in
-- R1/node.
--
-- The slow entry point is used for unknown calls: eg. stg_PAP_entry

mkSlowEntryCode :: ClosureInfo -> [LocalReg] -> FCode ()
-- If this function doesn't have a specialised ArgDescr, we need
-- to generate the function's arg bitmap and slow-entry code.
-- Here, we emit the slow-entry code.
mkSlowEntryCode _ [] = panic "entering a closure with no arguments?"
mkSlowEntryCode cl_info arg_regs -- function closure is already in `Node'
  | Just (_, ArgGen _) <- closureFunInfo cl_info
  = do let slow_lbl = closureSlowEntryLabel  cl_info
           fast_lbl = closureLocalEntryLabel cl_info
           -- mkDirectJump does not clobber `Node' containing function closure
           jump = mkDirectJump (mkLblExpr fast_lbl)
                               (map (CmmReg . CmmLocal) arg_regs)
                               initUpdFrameOff
       emitProcWithConvention Slow CmmNonInfoTable slow_lbl arg_regs jump
  | otherwise = return ()

-----------------------------------------
thunkCode :: ClosureInfo -> [(NonVoid Id, VirtualHpOffset)] -> CostCentreStack
          -> LocalReg -> Int -> StgExpr -> FCode ()
thunkCode cl_info fv_details _cc node arity body
  = do { let node_points = nodeMustPointToIt (closureLFInfo cl_info)
             node'       = if node_points then Just node else Nothing
        ; tickyEnterThunk cl_info
        ; ldvEnterClosure cl_info -- NB: Node always points when profiling
        ; granThunk node_points

        -- Heap overflow check
        ; entryHeapCheck cl_info 0 node' arity [] $ do
        { -- Overwrite with black hole if necessary
          -- but *after* the heap-overflow check
        ; whenC (blackHoleOnEntry cl_info && node_points)
                (blackHoleIt cl_info)

          -- Push update frame
        ; setupUpdate cl_info node $
            -- We only enter cc after setting up update so
            -- that cc of enclosing scope will be recorded
            -- in update frame CAF/DICT functions will be
            -- subsumed by this enclosing cc
            do { enterCostCentreThunk (CmmReg nodeReg)
               ; let lf_info = closureLFInfo cl_info
               ; fv_bindings <- mapM bind_fv fv_details
               ; load_fvs node lf_info fv_bindings
               ; cgExpr body }}}


------------------------------------------------------------------------
--		Update and black-hole wrappers
------------------------------------------------------------------------

blackHoleIt :: ClosureInfo -> FCode ()
-- Only called for closures with no args
-- Node points to the closure
blackHoleIt closure_info = emitBlackHoleCode (closureSingleEntry closure_info)

emitBlackHoleCode :: Bool -> FCode ()
emitBlackHoleCode is_single_entry = do
  dflags <- getDynFlags

  -- Eager blackholing is normally disabled, but can be turned on with
  -- -feager-blackholing.  When it is on, we replace the info pointer
  -- of the thunk with stg_EAGER_BLACKHOLE_info on entry.
  
  -- If we wanted to do eager blackholing with slop filling, we'd need
  -- to do it at the *end* of a basic block, otherwise we overwrite
  -- the free variables in the thunk that we still need.  We have a
  -- patch for this from Andy Cheadle, but not incorporated yet. --SDM
  -- [6/2004]
  --
  -- Previously, eager blackholing was enabled when ticky-ticky was
  -- on. But it didn't work, and it wasn't strictly necessary to bring
  -- back minimal ticky-ticky, so now EAGER_BLACKHOLING is
  -- unconditionally disabled. -- krc 1/2007
  
  -- Note the eager-blackholing check is here rather than in blackHoleOnEntry,
  -- because emitBlackHoleCode is called from CmmParse.

  let  eager_blackholing =  not opt_SccProfilingOn
                         && dopt Opt_EagerBlackHoling dflags
             -- Profiling needs slop filling (to support LDV
             -- profiling), so currently eager blackholing doesn't
             -- work with profiling.

  whenC eager_blackholing $ do
    tickyBlackHole (not is_single_entry)
    emitStore (cmmOffsetW (CmmReg nodeReg) fixedHdrSize)
                  (CmmReg (CmmGlobal CurrentTSO))
    emitPrimCall [] MO_WriteBarrier []
    emitStore (CmmReg nodeReg) (CmmReg (CmmGlobal EagerBlackholeInfo))

setupUpdate :: ClosureInfo -> LocalReg -> FCode () -> FCode ()
	-- Nota Bene: this function does not change Node (even if it's a CAF),
	-- so that the cost centre in the original closure can still be
	-- extracted by a subsequent enterCostCentre
setupUpdate closure_info node body
  | closureReEntrant closure_info
  = body

  | not (isStaticClosure closure_info)
  = if not (closureUpdReqd closure_info)
      then do tickyUpdateFrameOmitted; body
      else do
          tickyPushUpdateFrame
          dflags <- getDynFlags
          let
              bh = blackHoleOnEntry closure_info &&
                   not opt_SccProfilingOn && dopt Opt_EagerBlackHoling dflags

              lbl | bh        = mkBHUpdInfoLabel
                  | otherwise = mkUpdInfoLabel

          pushUpdateFrame [CmmReg (CmmLocal node), mkLblExpr lbl] body

  | otherwise	-- A static closure
  = do 	{ tickyUpdateBhCaf closure_info

	; if closureUpdReqd closure_info
	  then do	-- Blackhole the (updatable) CAF:
                { upd_closure <- link_caf True
		; pushUpdateFrame [CmmReg (CmmLocal upd_closure),
                                   mkLblExpr mkBHUpdInfoLabel] body }
	  else do {tickyUpdateFrameOmitted; body}
    }

-----------------------------------------------------------------------------
-- Setting up update frames

-- Push the update frame on the stack in the Entry area,
-- leaving room for the return address that is already
-- at the old end of the area.
pushUpdateFrame :: [CmmExpr] -> FCode () -> FCode ()
pushUpdateFrame es body
  = do -- [EZY] I'm not sure if we need to special-case for BH too
       updfr  <- getUpdFrameOff
       offset <- foldM push updfr es
       withUpdFrameOff offset body
     where push off e =
             do emitStore (CmmStackSlot Old base) e
                return base
             where base = off + widthInBytes (cmmExprWidth e)

-----------------------------------------------------------------------------
-- Entering a CAF
--
-- When a CAF is first entered, it creates a black hole in the heap,
-- and updates itself with an indirection to this new black hole.
--
-- We update the CAF with an indirection to a newly-allocated black
-- hole in the heap.  We also set the blocking queue on the newly
-- allocated black hole to be empty.
--
-- Why do we make a black hole in the heap when we enter a CAF?
--
--     - for a  generational garbage collector, which needs a fast
--       test for whether an updatee is in an old generation or not
--
--     - for the parallel system, which can implement updates more
--       easily if the updatee is always in the heap. (allegedly).
--
-- When debugging, we maintain a separate CAF list so we can tell when
-- a CAF has been garbage collected.

-- newCAF must be called before the itbl ptr is overwritten, since
-- newCAF records the old itbl ptr in order to do CAF reverting
-- (which Hugs needs to do in order that combined mode works right.)
--

-- ToDo [Feb 04]  This entire link_caf nonsense could all be moved
-- into the "newCAF" RTS procedure, which we call anyway, including
-- the allocation of the black-hole indirection closure.
-- That way, code size would fall, the CAF-handling code would
-- be closer together, and the compiler wouldn't need to know
-- about off_indirectee etc.

link_caf :: Bool                -- True <=> updatable, False <=> single-entry
         -> FCode LocalReg      -- Returns amode for closure to be updated
-- To update a CAF we must allocate a black hole, link the CAF onto the
-- CAF list, then update the CAF to point to the fresh black hole.
-- This function returns the address of the black hole, so it can be
-- updated with the new value when available.  The reason for all of this
-- is that we only want to update dynamic heap objects, not static ones,
-- so that generational GC is easier.
link_caf _is_upd = do
  { 	-- Alloc black hole specifying CC_HDR(Node) as the cost centre
  ; let	use_cc   = costCentreFrom (CmmReg nodeReg)
        blame_cc = use_cc
        tso      = CmmReg (CmmGlobal CurrentTSO)

  ; (hp_rel, init) <- allocDynClosureCmm cafBlackHoleInfoTable mkLFBlackHole
                                         use_cc blame_cc [(tso,fixedHdrSize)]
  ; emit init

	-- Call the RTS function newCAF to add the CAF to the CafList
	-- so that the garbage collector can find them
	-- This must be done *before* the info table pointer is overwritten,
	-- because the old info table ptr is needed for reversion
  ; ret <- newTemp bWord
  ; emitRtsCallGen [(ret,NoHint)] rtsPackageId (fsLit "newCAF")
      [ (CmmReg (CmmGlobal BaseReg),  AddrHint),
        (CmmReg nodeReg, AddrHint),
        (CmmReg (CmmLocal hp_rel), AddrHint) ]
      (Just [node]) False
        -- node is live, so save it.

  -- see Note [atomic CAF entry] in rts/sm/Storage.c
  ; updfr  <- getUpdFrameOff
  ; emit =<< mkCmmIfThen
      (CmmMachOp mo_wordEq [ CmmReg (CmmLocal ret), CmmLit zeroCLit])
        -- re-enter R1.  Doing this directly is slightly dodgy; we're
        -- assuming lots of things, like the stack pointer hasn't
        -- moved since we entered the CAF.
       (let target = entryCode (closureInfoPtr (CmmReg nodeReg)) in
        mkJump target [] updfr)

  ; return hp_rel }

------------------------------------------------------------------------
--		Profiling
------------------------------------------------------------------------

-- For "global" data constructors the description is simply occurrence
-- name of the data constructor itself.  Otherwise it is determined by
-- @closureDescription@ from the let binding information.

closureDescription :: DynFlags
           -> Module		-- Module
		   -> Name		-- Id of closure binding
		   -> String
	-- Not called for StgRhsCon which have global info tables built in
	-- CgConTbls.lhs with a description generated from the data constructor
closureDescription dflags mod_name name
  = showSDocDump dflags (char '<' <>
		    (if isExternalName name
		      then ppr name -- ppr will include the module name prefix
		      else pprModule mod_name <> char '.' <> ppr name) <>
		    char '>')
   -- showSDocDump, because we want to see the unique on the Name.


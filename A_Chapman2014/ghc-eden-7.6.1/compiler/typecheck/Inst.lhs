%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

The @Inst@ type: dictionaries or method instances

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module Inst ( 
       deeplySkolemise, 
       deeplyInstantiate, instCall, instStupidTheta,
       emitWanted, emitWanteds,

       newOverloadedLit, mkOverLit, 
     
       tcGetInstEnvs, getOverlapFlag,
       tcExtendLocalInstEnv, instCallConstraints, newMethodFromName,
       tcSyntaxName,

       -- Simple functions over evidence variables
       hasEqualities, unitImplication,
       
       tyVarsOfWC, tyVarsOfBag, 
       tyVarsOfEvVar, tyVarsOfEvVars, tyVarsOfImplication,
       tyVarsOfCt, tyVarsOfCts, tyVarsOfCDict, tyVarsOfCDicts,

       tidyEvVar, tidyCt, tidyGivenLoc,

       substEvVar, substImplication, substCt
    ) where

#include "HsVersions.h"

import {-# SOURCE #-}	TcExpr( tcPolyExpr, tcSyntaxOp )
import {-# SOURCE #-}	TcUnify( unifyType )

import FastString
import HsSyn
import TcHsSyn
import TcRnMonad
import TcEnv
import TcEvidence
import InstEnv
import FunDeps
import TcMType
import Type
import TcType
import Class
import Unify
import HscTypes
import Id
import Name
import Var      ( Var, EvVar, varType, setVarType )
import VarEnv
import VarSet
import PrelNames
import SrcLoc
import DynFlags
import Bag
import Maybes
import Util
import Outputable
import Data.List( mapAccumL )
\end{code}



%************************************************************************
%*									*
		Emitting constraints
%*									*
%************************************************************************

\begin{code}
emitWanteds :: CtOrigin -> TcThetaType -> TcM [EvVar]
emitWanteds origin theta = mapM (emitWanted origin) theta

emitWanted :: CtOrigin -> TcPredType -> TcM EvVar
emitWanted origin pred 
  = do { loc <- getCtLoc origin
       ; ev  <- newWantedEvVar pred
       ; emitFlat (mkNonCanonical (Wanted { ctev_wloc = loc, ctev_pred = pred, ctev_evar = ev }))
       ; return ev }

newMethodFromName :: CtOrigin -> Name -> TcRhoType -> TcM (HsExpr TcId)
-- Used when Name is the wired-in name for a wired-in class method,
-- so the caller knows its type for sure, which should be of form
--    forall a. C a => <blah>
-- newMethodFromName is supposed to instantiate just the outer 
-- type variable and constraint

newMethodFromName origin name inst_ty
  = do { id <- tcLookupId name
 	      -- Use tcLookupId not tcLookupGlobalId; the method is almost
	      -- always a class op, but with -XRebindableSyntax GHC is
	      -- meant to find whatever thing is in scope, and that may
	      -- be an ordinary function. 

       ; let (tvs, theta, _caller_knows_this) = tcSplitSigmaTy (idType id)
             (the_tv:rest) = tvs
             subst = zipOpenTvSubst [the_tv] [inst_ty]

       ; wrap <- ASSERT( null rest && isSingleton theta )
                 instCall origin [inst_ty] (substTheta subst theta)
       ; return (mkHsWrap wrap (HsVar id)) }
\end{code}


%************************************************************************
%*									*
	Deep instantiation and skolemisation
%*									*
%************************************************************************

Note [Deep skolemisation]
~~~~~~~~~~~~~~~~~~~~~~~~~
deeplySkolemise decomposes and skolemises a type, returning a type
with all its arrows visible (ie not buried under foralls)

Examples:

  deeplySkolemise (Int -> forall a. Ord a => blah)  
    =  ( wp, [a], [d:Ord a], Int -> blah )
    where wp = \x:Int. /\a. \(d:Ord a). <hole> x

  deeplySkolemise  (forall a. Ord a => Maybe a -> forall b. Eq b => blah)  
    =  ( wp, [a,b], [d1:Ord a,d2:Eq b], Maybe a -> blah )
    where wp = /\a.\(d1:Ord a).\(x:Maybe a)./\b.\(d2:Ord b). <hole> x

In general,
  if      deeplySkolemise ty = (wrap, tvs, evs, rho)
    and   e :: rho
  then    wrap e :: ty
    and   'wrap' binds tvs, evs

ToDo: this eta-abstraction plays fast and loose with termination,
      because it can introduce extra lambdas.  Maybe add a `seq` to
      fix this


\begin{code}
deeplySkolemise
  :: TcSigmaType
  -> TcM (HsWrapper, [TyVar], [EvVar], TcRhoType)

deeplySkolemise ty
  | Just (arg_tys, tvs, theta, ty') <- tcDeepSplitSigmaTy_maybe ty
  = do { ids1 <- newSysLocalIds (fsLit "dk") arg_tys
       ; (subst, tvs1) <- tcInstSkolTyVars tvs
       ; ev_vars1 <- newEvVars (substTheta subst theta)
       ; (wrap, tvs2, ev_vars2, rho) <- deeplySkolemise (substTy subst ty')
       ; return ( mkWpLams ids1
                   <.> mkWpTyLams tvs1
                   <.> mkWpLams ev_vars1
                   <.> wrap
                   <.> mkWpEvVarApps ids1
                , tvs1     ++ tvs2
                , ev_vars1 ++ ev_vars2
                , mkFunTys arg_tys rho ) }

  | otherwise
  = return (idHsWrapper, [], [], ty)

deeplyInstantiate :: CtOrigin -> TcSigmaType -> TcM (HsWrapper, TcRhoType)
--   Int -> forall a. a -> a  ==>  (\x:Int. [] x alpha) :: Int -> alpha
-- In general if
-- if    deeplyInstantiate ty = (wrap, rho)
-- and   e :: ty
-- then  wrap e :: rho

deeplyInstantiate orig ty
  | Just (arg_tys, tvs, theta, rho) <- tcDeepSplitSigmaTy_maybe ty
  = do { (_, tys, subst) <- tcInstTyVars tvs
       ; ids1  <- newSysLocalIds (fsLit "di") (substTys subst arg_tys)
       ; wrap1 <- instCall orig tys (substTheta subst theta)
       ; (wrap2, rho2) <- deeplyInstantiate orig (substTy subst rho)
       ; return (mkWpLams ids1 
                    <.> wrap2
                    <.> wrap1 
                    <.> mkWpEvVarApps ids1,
                 mkFunTys arg_tys rho2) }

  | otherwise = return (idHsWrapper, ty)
\end{code}


%************************************************************************
%*									*
            Instantiating a call
%*									*
%************************************************************************

\begin{code}
----------------
instCall :: CtOrigin -> [TcType] -> TcThetaType -> TcM HsWrapper
-- Instantiate the constraints of a call
--	(instCall o tys theta)
-- (a) Makes fresh dictionaries as necessary for the constraints (theta)
-- (b) Throws these dictionaries into the LIE
-- (c) Returns an HsWrapper ([.] tys dicts)

instCall orig tys theta 
  = do	{ dict_app <- instCallConstraints orig theta
	; return (dict_app <.> mkWpTyApps tys) }

----------------
instCallConstraints :: CtOrigin -> TcThetaType -> TcM HsWrapper
-- Instantiates the TcTheta, puts all constraints thereby generated
-- into the LIE, and returns a HsWrapper to enclose the call site.

instCallConstraints _ [] = return idHsWrapper

instCallConstraints origin (pred : preds)
  | Just (ty1, ty2) <- getEqPredTys_maybe pred -- Try short-cut
  = do  { traceTc "instCallConstraints" $ ppr (mkEqPred ty1 ty2)
        ; co <- unifyType ty1 ty2
	; co_fn <- instCallConstraints origin preds
        ; return (co_fn <.> WpEvApp (EvCoercion co)) }

  | otherwise
  = do	{ ev_var <- emitWanted origin pred
	; co_fn <- instCallConstraints origin preds
	; return (co_fn <.> WpEvApp (EvId ev_var)) }

----------------
instStupidTheta :: CtOrigin -> TcThetaType -> TcM ()
-- Similar to instCall, but only emit the constraints in the LIE
-- Used exclusively for the 'stupid theta' of a data constructor
instStupidTheta orig theta
  = do	{ _co <- instCallConstraints orig theta -- Discard the coercion
	; return () }
\end{code}

%************************************************************************
%*									*
		Literals
%*									*
%************************************************************************

In newOverloadedLit we convert directly to an Int or Integer if we
know that's what we want.  This may save some time, by not
temporarily generating overloaded literals, but it won't catch all
cases (the rest are caught in lookupInst).

\begin{code}
newOverloadedLit :: CtOrigin
		 -> HsOverLit Name
		 -> TcRhoType
		 -> TcM (HsOverLit TcId)
newOverloadedLit orig 
  lit@(OverLit { ol_val = val, ol_rebindable = rebindable
	       , ol_witness = meth_name }) res_ty

  | not rebindable
  , Just expr <- shortCutLit val res_ty 
	-- Do not generate a LitInst for rebindable syntax.  
	-- Reason: If we do, tcSimplify will call lookupInst, which
	--	   will call tcSyntaxName, which does unification, 
	--	   which tcSimplify doesn't like
  = return (lit { ol_witness = expr, ol_type = res_ty })

  | otherwise
  = do	{ hs_lit <- mkOverLit val
	; let lit_ty = hsLitType hs_lit
	; fi' <- tcSyntaxOp orig meth_name (mkFunTy lit_ty res_ty)
	 	-- Overloaded literals must have liftedTypeKind, because
	 	-- we're instantiating an overloaded function here,
	 	-- whereas res_ty might be openTypeKind. This was a bug in 6.2.2
		-- However this'll be picked up by tcSyntaxOp if necessary
	; let witness = HsApp (noLoc fi') (noLoc (HsLit hs_lit))
	; return (lit { ol_witness = witness, ol_type = res_ty }) }

------------
mkOverLit :: OverLitVal -> TcM HsLit
mkOverLit (HsIntegral i) 
  = do	{ integer_ty <- tcMetaTy integerTyConName
	; return (HsInteger i integer_ty) }

mkOverLit (HsFractional r)
  = do	{ rat_ty <- tcMetaTy rationalTyConName
	; return (HsRat r rat_ty) }

mkOverLit (HsIsString s) = return (HsString s)
\end{code}




%************************************************************************
%*									*
		Re-mappable syntax
    
     Used only for arrow syntax -- find a way to nuke this
%*									*
%************************************************************************

Suppose we are doing the -XRebindableSyntax thing, and we encounter
a do-expression.  We have to find (>>) in the current environment, which is
done by the rename. Then we have to check that it has the same type as
Control.Monad.(>>).  Or, more precisely, a compatible type. One 'customer' had
this:

  (>>) :: HB m n mn => m a -> n b -> mn b

So the idea is to generate a local binding for (>>), thus:

	let then72 :: forall a b. m a -> m b -> m b
	    then72 = ...something involving the user's (>>)...
	in
	...the do-expression...

Now the do-expression can proceed using then72, which has exactly
the expected type.

In fact tcSyntaxName just generates the RHS for then72, because we only
want an actual binding in the do-expression case. For literals, we can 
just use the expression inline.

\begin{code}
tcSyntaxName :: CtOrigin
	     -> TcType			-- Type to instantiate it at
	     -> (Name, HsExpr Name)	-- (Standard name, user name)
	     -> TcM (Name, HsExpr TcId)	-- (Standard name, suitable expression)
--	*** NOW USED ONLY FOR CmdTop (sigh) ***
-- NB: tcSyntaxName calls tcExpr, and hence can do unification.
-- So we do not call it from lookupInst, which is called from tcSimplify

tcSyntaxName orig ty (std_nm, HsVar user_nm)
  | std_nm == user_nm
  = do rhs <- newMethodFromName orig std_nm ty
       return (std_nm, rhs)

tcSyntaxName orig ty (std_nm, user_nm_expr) = do
    std_id <- tcLookupId std_nm
    let	
	-- C.f. newMethodAtLoc
	([tv], _, tau)  = tcSplitSigmaTy (idType std_id)
 	sigma1		= substTyWith [tv] [ty] tau
	-- Actually, the "tau-type" might be a sigma-type in the
	-- case of locally-polymorphic methods.

    addErrCtxtM (syntaxNameCtxt user_nm_expr orig sigma1) $ do

	-- Check that the user-supplied thing has the
	-- same type as the standard one.  
	-- Tiresome jiggling because tcCheckSigma takes a located expression
     span <- getSrcSpanM
     expr <- tcPolyExpr (L span user_nm_expr) sigma1
     return (std_nm, unLoc expr)

syntaxNameCtxt :: HsExpr Name -> CtOrigin -> Type -> TidyEnv
               -> TcRn (TidyEnv, SDoc)
syntaxNameCtxt name orig ty tidy_env = do
    inst_loc <- getCtLoc orig
    let
	msg = vcat [ptext (sLit "When checking that") <+> quotes (ppr name) <+> 
				ptext (sLit "(needed by a syntactic construct)"),
		    nest 2 (ptext (sLit "has the required type:") <+> ppr (tidyType tidy_env ty)),
		    nest 2 (pprArisingAt inst_loc)]
    return (tidy_env, msg)
\end{code}


%************************************************************************
%*									*
		Instances
%*									*
%************************************************************************

\begin{code}
getOverlapFlag :: TcM OverlapFlag
getOverlapFlag 
  = do  { dflags <- getDynFlags
        ; let overlap_ok    = xopt Opt_OverlappingInstances dflags
              incoherent_ok = xopt Opt_IncoherentInstances  dflags
              safeOverlap   = safeLanguageOn dflags
              overlap_flag | incoherent_ok = Incoherent safeOverlap
                           | overlap_ok    = OverlapOk safeOverlap
                           | otherwise     = NoOverlap safeOverlap

        ; return overlap_flag }

tcGetInstEnvs :: TcM (InstEnv, InstEnv)
-- Gets both the external-package inst-env
-- and the home-pkg inst env (includes module being compiled)
tcGetInstEnvs = do { eps <- getEps; env <- getGblEnv;
		     return (eps_inst_env eps, tcg_inst_env env) }

tcExtendLocalInstEnv :: [ClsInst] -> TcM a -> TcM a
  -- Add new locally-defined instances
tcExtendLocalInstEnv dfuns thing_inside
 = do { traceDFuns dfuns
      ; env <- getGblEnv
      ; inst_env' <- foldlM addLocalInst (tcg_inst_env env) dfuns
      ; let env' = env { tcg_insts = dfuns ++ tcg_insts env,
			 tcg_inst_env = inst_env' }
      ; setGblEnv env' thing_inside }

addLocalInst :: InstEnv -> ClsInst -> TcM InstEnv
-- Check that the proposed new instance is OK, 
-- and then add it to the home inst env
-- If overwrite_inst, then we can overwrite a direct match
addLocalInst home_ie ispec = do
    -- Instantiate the dfun type so that we extend the instance
    -- envt with completely fresh template variables
    -- This is important because the template variables must
    -- not overlap with anything in the things being looked up
    -- (since we do unification).  
        --
        -- We use tcInstSkolType because we don't want to allocate fresh
        --  *meta* type variables.
        --
        -- We use UnkSkol --- and *not* InstSkol or PatSkol --- because
        -- these variables must be bindable by tcUnifyTys.  See
        -- the call to tcUnifyTys in InstEnv, and the special
        -- treatment that instanceBindFun gives to isOverlappableTyVar
        -- This is absurdly delicate.

    let dfun = instanceDFunId ispec
    (tvs', theta', tau') <- tcInstSkolType (idType dfun)
    let (cls, tys') = tcSplitDFunHead tau'
        dfun' 	    = setIdType dfun (mkSigmaTy tvs' theta' tau')	    
        ispec'      = setInstanceDFunId ispec dfun'

        -- Load imported instances, so that we report
        -- duplicates correctly
    eps <- getEps
    let inst_envs = (eps_inst_env eps, home_ie)

        -- Check functional dependencies
    case checkFunDeps inst_envs ispec' of
        Just specs -> funDepErr ispec' specs
        Nothing    -> return ()

        -- Check for duplicate instance decls
    let (matches, unifs, _) = lookupInstEnv inst_envs cls tys'
        dup_ispecs = [ dup_ispec 
                        | (dup_ispec, _) <- matches
                        , let (_,_,_,dup_tys) = instanceHead dup_ispec
                        , isJust (tcMatchTys (mkVarSet tvs') tys' dup_tys)]
                        
        -- Find memebers of the match list which ispec itself matches.
        -- If the match is 2-way, it's a duplicate
        -- If it's a duplicate, but we can overwrite home package dups, then overwrite
    isGHCi <- getIsGHCi
    overlapFlag <- getOverlapFlag
    case isGHCi of
        False -> case dup_ispecs of
            dup : _ -> dupInstErr ispec' dup >> return (extendInstEnv home_ie ispec')
            []      -> return (extendInstEnv home_ie ispec')
        True  -> case (dup_ispecs, home_ie_matches, unifs, overlapFlag) of
            (_, _:_, _, _)      -> return (overwriteInstEnv home_ie ispec')
            (dup:_, [], _, _)   -> dupInstErr ispec' dup >> return (extendInstEnv home_ie ispec')
            ([], _, u:_, NoOverlap _)    -> overlappingInstErr ispec' u >> return (extendInstEnv home_ie ispec')
            _                   -> return (extendInstEnv home_ie ispec')
          where (homematches, _) = lookupInstEnv' home_ie cls tys'
                home_ie_matches = [ dup_ispec 
                    | (dup_ispec, _) <- homematches
                    , let (_,_,_,dup_tys) = instanceHead dup_ispec
                    , isJust (tcMatchTys (mkVarSet tvs') tys' dup_tys)]

traceDFuns :: [ClsInst] -> TcRn ()
traceDFuns ispecs
  = traceTc "Adding instances:" (vcat (map pp ispecs))
  where
    pp ispec = ppr (instanceDFunId ispec) <+> colon <+> ppr ispec
	-- Print the dfun name itself too

funDepErr :: ClsInst -> [ClsInst] -> TcRn ()
funDepErr ispec ispecs
  = addClsInstsErr (ptext (sLit "Functional dependencies conflict between instance declarations:"))
                    (ispec : ispecs)

dupInstErr :: ClsInst -> ClsInst -> TcRn ()
dupInstErr ispec dup_ispec
  = addClsInstsErr (ptext (sLit "Duplicate instance declarations:"))
	            [ispec, dup_ispec]

overlappingInstErr :: ClsInst -> ClsInst -> TcRn ()
overlappingInstErr ispec dup_ispec
  = addClsInstsErr (ptext (sLit "Overlapping instance declarations:")) 
                    [ispec, dup_ispec]

addClsInstsErr :: SDoc -> [ClsInst] -> TcRn ()
addClsInstsErr herald ispecs
  = setSrcSpan (getSrcSpan (head sorted)) $
    addErr (hang herald 2 (pprInstances sorted))
 where
   sorted = sortWith getSrcLoc ispecs
   -- The sortWith just arranges that instances are dislayed in order
   -- of source location, which reduced wobbling in error messages,
   -- and is better for users
\end{code}

%************************************************************************
%*									*
	Simple functions over evidence variables
%*									*
%************************************************************************

\begin{code}
unitImplication :: Implication -> Bag Implication
unitImplication implic
  | isEmptyWC (ic_wanted implic) = emptyBag
  | otherwise                    = unitBag implic

hasEqualities :: [EvVar] -> Bool
-- Has a bunch of canonical constraints (all givens) got any equalities in it?
hasEqualities givens = any (has_eq . evVarPred) givens
  where
    has_eq = has_eq' . classifyPredType
    
    -- See Note [Float Equalities out of Implications] in TcSimplify
    has_eq' (EqPred {})          = True
    has_eq' (ClassPred cls _tys) = any has_eq (classSCTheta cls)
    has_eq' (TuplePred ts)       = any has_eq ts
    has_eq' (IrredPred _)        = True -- Might have equalities in it after reduction?

---------------- Getting free tyvars -------------------------
tyVarsOfCt :: Ct -> TcTyVarSet
tyVarsOfCt (CTyEqCan { cc_tyvar = tv, cc_rhs = xi })    = extendVarSet (tyVarsOfType xi) tv
tyVarsOfCt (CFunEqCan { cc_tyargs = tys, cc_rhs = xi }) = tyVarsOfTypes (xi:tys)
tyVarsOfCt (CDictCan { cc_tyargs = tys }) 	        = tyVarsOfTypes tys
tyVarsOfCt (CIrredEvCan { cc_ty = ty })                 = tyVarsOfType ty
tyVarsOfCt (CNonCanonical { cc_ev = fl })           = tyVarsOfType (ctEvPred fl)

tyVarsOfCDict :: Ct -> TcTyVarSet 
tyVarsOfCDict (CDictCan { cc_tyargs = tys }) = tyVarsOfTypes tys
tyVarsOfCDict _ct                            = emptyVarSet 

tyVarsOfCDicts :: Cts -> TcTyVarSet 
tyVarsOfCDicts = foldrBag (unionVarSet . tyVarsOfCDict) emptyVarSet

tyVarsOfCts :: Cts -> TcTyVarSet
tyVarsOfCts = foldrBag (unionVarSet . tyVarsOfCt) emptyVarSet

tyVarsOfWC :: WantedConstraints -> TyVarSet
tyVarsOfWC (WC { wc_flat = flat, wc_impl = implic, wc_insol = insol })
  = tyVarsOfCts flat `unionVarSet`
    tyVarsOfBag tyVarsOfImplication implic `unionVarSet`
    tyVarsOfCts insol

tyVarsOfImplication :: Implication -> TyVarSet
tyVarsOfImplication (Implic { ic_skols = skols, ic_wanted = wanted })
  = tyVarsOfWC wanted `delVarSetList` skols

tyVarsOfEvVar :: EvVar -> TyVarSet
tyVarsOfEvVar ev = tyVarsOfType $ evVarPred ev

tyVarsOfEvVars :: [EvVar] -> TyVarSet
tyVarsOfEvVars = foldr (unionVarSet . tyVarsOfEvVar) emptyVarSet

tyVarsOfBag :: (a -> TyVarSet) -> Bag a -> TyVarSet
tyVarsOfBag tvs_of = foldrBag (unionVarSet . tvs_of) emptyVarSet

---------------- Tidying -------------------------

tidyCt :: TidyEnv -> Ct -> Ct
-- Used only in error reporting
-- Also converts it to non-canonical
tidyCt env ct 
  = CNonCanonical { cc_ev = tidy_flavor env (cc_ev ct)
                  , cc_depth  = cc_depth ct } 
  where 
    tidy_flavor :: TidyEnv -> CtEvidence -> CtEvidence
     -- NB: we do not tidy the ctev_evtm/var field because we don't 
     --     show it in error messages
    tidy_flavor env ctev@(Given { ctev_gloc = gloc, ctev_pred = pred })
      = ctev { ctev_gloc = tidyGivenLoc env gloc
             , ctev_pred = tidyType env pred }
    tidy_flavor env ctev@(Wanted { ctev_pred = pred })
      = ctev { ctev_pred = tidyType env pred }
    tidy_flavor env ctev@(Derived { ctev_pred = pred })
      = ctev { ctev_pred = tidyType env pred }

tidyEvVar :: TidyEnv -> EvVar -> EvVar
tidyEvVar env var = setVarType var (tidyType env (varType var))

tidyGivenLoc :: TidyEnv -> GivenLoc -> GivenLoc
tidyGivenLoc env (CtLoc skol span ctxt) 
  = CtLoc (tidySkolemInfo env skol) span ctxt

tidySkolemInfo :: TidyEnv -> SkolemInfo -> SkolemInfo
tidySkolemInfo env (SigSkol cx ty) = SigSkol cx (tidyType env ty)
tidySkolemInfo env (InferSkol ids) = InferSkol (mapSnd (tidyType env) ids)
tidySkolemInfo env (UnifyForAllSkol skol_tvs ty) 
  = UnifyForAllSkol (map tidy_tv skol_tvs) (tidyType env ty)
  where
    tidy_tv tv = case getTyVar_maybe ty' of
                   Just tv' -> tv'
                   Nothing  -> pprPanic "ticySkolemInfo" (ppr tv <+> ppr ty')
               where
                 ty' = tidyTyVarOcc env tv
tidySkolemInfo _   info            = info

---------------- Substitution -------------------------
-- This is used only in TcSimpify, for substituations that are *also* 
-- reflected in the unification variables.  So we don't substitute
-- in the evidence.

substCt :: TvSubst -> Ct -> Ct 
-- Conservatively converts it to non-canonical:
-- Postcondition: if the constraint does not get rewritten
substCt subst ct
  | pty <- ctPred ct
  , sty <- substTy subst pty 
  = if sty `eqType` pty then 
        ct { cc_ev = substFlavor subst (cc_ev ct) }
    else 
        CNonCanonical { cc_ev = substFlavor subst (cc_ev ct)
                      , cc_depth  = cc_depth ct }

substWC :: TvSubst -> WantedConstraints -> WantedConstraints
substWC subst (WC { wc_flat = flat, wc_impl = implic, wc_insol = insol })
  = WC { wc_flat  = mapBag (substCt subst) flat
       , wc_impl  = mapBag (substImplication subst) implic
       , wc_insol = mapBag (substCt subst) insol }

substImplication :: TvSubst -> Implication -> Implication
substImplication subst implic@(Implic { ic_skols = tvs
                                      , ic_given = given
                                      , ic_wanted = wanted
                                      , ic_loc = loc })
  = implic { ic_skols  = tvs'
           , ic_given  = map (substEvVar subst1) given
           , ic_wanted = substWC subst1 wanted
           , ic_loc    = substGivenLoc subst1 loc }
  where
   (subst1, tvs') = mapAccumL substTyVarBndr subst tvs

substEvVar :: TvSubst -> EvVar -> EvVar
substEvVar subst var = setVarType var (substTy subst (varType var))

substFlavor :: TvSubst -> CtEvidence -> CtEvidence
substFlavor subst ctev@(Given { ctev_gloc = gloc, ctev_pred = pred })
  = ctev { ctev_gloc = substGivenLoc subst gloc
          , ctev_pred = substTy subst pred }

substFlavor subst ctev@(Wanted { ctev_pred = pred })
  = ctev  { ctev_pred = substTy subst pred }

substFlavor subst ctev@(Derived { ctev_pred = pty })
  = ctev { ctev_pred = substTy subst pty }

substGivenLoc :: TvSubst -> GivenLoc -> GivenLoc
substGivenLoc subst (CtLoc skol span ctxt) 
  = CtLoc (substSkolemInfo subst skol) span ctxt

substSkolemInfo :: TvSubst -> SkolemInfo -> SkolemInfo
substSkolemInfo subst (SigSkol cx ty) = SigSkol cx (substTy subst ty)
substSkolemInfo subst (InferSkol ids) = InferSkol (mapSnd (substTy subst) ids)
substSkolemInfo _     info            = info
\end{code}

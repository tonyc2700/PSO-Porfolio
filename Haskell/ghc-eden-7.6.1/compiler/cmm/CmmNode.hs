-- CmmNode type for representation using Hoopl graphs.
{-# LANGUAGE GADTs #-}

{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#if __GLASGOW_HASKELL__ >= 703
-- GHC 7.0.1 improved incomplete pattern warnings with GADTs
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
#endif

module CmmNode (
     CmmNode(..), ForeignHint(..), CmmFormal, CmmActual,
     UpdFrameOffset, Convention(..), ForeignConvention(..), ForeignTarget(..),
     mapExp, mapExpDeep, wrapRecExp, foldExp, foldExpDeep, wrapRecExpf,
     mapExpM, mapExpDeepM, wrapRecExpM, mapSuccessors
  ) where

import CmmExpr
import FastString
import ForeignCall
import SMRep

import Compiler.Hoopl
import Data.Maybe
import Data.List (tails)
import Prelude hiding (succ)


------------------------
-- CmmNode

#define ULabel {-# UNPACK #-} !Label

data CmmNode e x where
  CmmEntry :: ULabel -> CmmNode C O

  CmmComment :: FastString -> CmmNode O O

  CmmAssign :: !CmmReg -> !CmmExpr -> CmmNode O O
    -- Assign to register

  CmmStore :: !CmmExpr -> !CmmExpr -> CmmNode O O
    -- Assign to memory location.  Size is
    -- given by cmmExprType of the rhs.

  CmmUnsafeForeignCall ::         -- An unsafe foreign call;
                                  -- see Note [Foreign calls]
  		       		  -- Like a "fat machine instruction"; can occur
				  -- in the middle of a block
      ForeignTarget ->            -- call target
      [CmmFormal] ->               -- zero or more results
      [CmmActual] ->               -- zero or more arguments
      CmmNode O O
      -- Semantics: kills only result regs; all other regs (both GlobalReg
      --            and LocalReg) are preserved.  But there is a current
      --            bug for what can be put in arguments, see
      --            Note [Register Parameter Passing]

  CmmBranch :: ULabel -> CmmNode O C
                                   -- Goto another block in the same procedure

  CmmCondBranch :: {                 -- conditional branch
      cml_pred :: CmmExpr,
      cml_true, cml_false :: ULabel
  } -> CmmNode O C

  CmmSwitch :: CmmExpr -> [Maybe Label] -> CmmNode O C -- Table branch
      -- The scrutinee is zero-based;
      --      zero -> first block
      --      one  -> second block etc
      -- Undefined outside range, and when there's a Nothing

  CmmCall :: {                -- A native call or tail call
      cml_target :: CmmExpr,  -- never a CmmPrim to a CallishMachOp!

      cml_cont :: Maybe Label,
          -- Label of continuation (Nothing for return or tail call)
          --
          -- Note [Continuation BlockId]: these BlockIds are called
          -- Continuation BlockIds, and are the only BlockIds that can
          -- occur in CmmExprs, namely as (CmmLit (CmmBlock b)) or
          -- (CmmStackSlot (Young b) _).

      cml_args_regs :: [GlobalReg],
          -- The argument GlobalRegs (Rx, Fx, Dx, Lx) that are passed
          -- to the call.  This is essential information for the
          -- native code generator's register allocator; without
          -- knowing which GlobalRegs are live it has to assume that
          -- they are all live.  This list should only include
          -- GlobalRegs that are mapped to real machine registers on
          -- the target platform.

      cml_args :: ByteOff,
          -- Byte offset, from the *old* end of the Area associated with
          -- the Label (if cml_cont = Nothing, then Old area), of
          -- youngest outgoing arg.  Set the stack pointer to this before
          -- transferring control.
          -- (NB: an update frame might also have been stored in the Old
          --      area, but it'll be in an older part than the args.)

      cml_ret_args :: ByteOff,
          -- For calls *only*, the byte offset for youngest returned value
          -- This is really needed at the *return* point rather than here
          -- at the call, but in practice it's convenient to record it here.

      cml_ret_off :: ByteOff
        -- For calls *only*, the byte offset of the base of the frame that
        -- must be described by the info table for the return point.
        -- The older words are an update frames, which have their own
        -- info-table and layout information

        -- From a liveness point of view, the stack words older than
        -- cml_ret_off are treated as live, even if the sequel of
        -- the call goes into a loop.
  } -> CmmNode O C

  CmmForeignCall :: {           -- A safe foreign call; see Note [Foreign calls]
  		    		-- Always the last node of a block
      tgt   :: ForeignTarget,   -- call target and convention
      res   :: [CmmFormal],     -- zero or more results
      args  :: [CmmActual],     -- zero or more arguments; see Note [Register parameter passing]
      succ  :: ULabel,          -- Label of continuation
      updfr :: UpdFrameOffset,  -- where the update frame is (for building infotable)
      intrbl:: Bool             -- whether or not the call is interruptible
  } -> CmmNode O C

{- Note [Foreign calls]
~~~~~~~~~~~~~~~~~~~~~~~
A CmmUnsafeForeignCall is used for *unsafe* foreign calls;
a CmmForeignCall call is used for *safe* foreign calls.

Unsafe ones are mostly easy: think of them as a "fat machine
instruction".  In particular, they do *not* kill all live registers,
just the registers they return to (there was a bit of code in GHC that
conservatively assumed otherwise.)  However, see [Register parameter passing].

Safe ones are trickier.  A safe foreign call 
     r = f(x)
ultimately expands to
     push "return address"	-- Never used to return to; 
     	  	  		-- just points an info table
     save registers into TSO
     call suspendThread
     r = f(x)			-- Make the call
     call resumeThread
     restore registers
     pop "return address"
We cannot "lower" a safe foreign call to this sequence of Cmms, because
after we've saved Sp all the Cmm optimiser's assumptions are broken.
Furthermore, currently the smart Cmm constructors know the calling
conventions for Haskell, the garbage collector, etc, and "lower" them
so that a LastCall passes no parameters or results.  But the smart 
constructors do *not* (currently) know the foreign call conventions.

Note that a safe foreign call needs an info table.
-}

{- Note [Register parameter passing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
On certain architectures, some registers are utilized for parameter
passing in the C calling convention.  For example, in x86-64 Linux
convention, rdi, rsi, rdx and rcx (as well as r8 and r9) may be used for
argument passing.  These are registers R3-R6, which our generated
code may also be using; as a result, it's necessary to save these
values before doing a foreign call.  This is done during initial
code generation in callerSaveVolatileRegs in StgCmmUtils.hs.  However,
one result of doing this is that the contents of these registers
may mysteriously change if referenced inside the arguments.  This
is dangerous, so you'll need to disable inlining much in the same
way is done in cmm/CmmOpt.hs currently.  We should fix this!
-}

---------------------------------------------
-- Eq instance of CmmNode
-- It is a shame GHC cannot infer it by itself :(

instance Eq (CmmNode e x) where
  (CmmEntry a)                 == (CmmEntry a')                   = a==a'
  (CmmComment a)               == (CmmComment a')                 = a==a'
  (CmmAssign a b)              == (CmmAssign a' b')               = a==a' && b==b'
  (CmmStore a b)               == (CmmStore a' b')                = a==a' && b==b'
  (CmmUnsafeForeignCall a b c) == (CmmUnsafeForeignCall a' b' c') = a==a' && b==b' && c==c'
  (CmmBranch a)                == (CmmBranch a')                  = a==a'
  (CmmCondBranch a b c)        == (CmmCondBranch a' b' c')        = a==a' && b==b' && c==c'
  (CmmSwitch a b)              == (CmmSwitch a' b')               = a==a' && b==b'
  (CmmCall a b c d e f)          == (CmmCall a' b' c' d' e' f')   = a==a' && b==b' && c==c' && d==d' && e==e' && f==f'
  (CmmForeignCall a b c d e f) == (CmmForeignCall a' b' c' d' e' f') = a==a' && b==b' && c==c' && d==d' && e==e' && f==f'
  _                            == _                               = False

----------------------------------------------
-- Hoopl instances of CmmNode

instance NonLocal CmmNode where
  entryLabel (CmmEntry l) = l

  successors (CmmBranch l) = [l]
  successors (CmmCondBranch {cml_true=t, cml_false=f}) = [f, t] -- meets layout constraint
  successors (CmmSwitch _ ls) = catMaybes ls
  successors (CmmCall {cml_cont=l}) = maybeToList l
  successors (CmmForeignCall {succ=l}) = [l]


--------------------------------------------------
-- Various helper types

type CmmActual = CmmExpr
type CmmFormal = LocalReg

type UpdFrameOffset = ByteOff

data Convention
  = NativeDirectCall -- Native C-- call skipping the node (closure) argument
  | NativeNodeCall   -- Native C-- call including the node argument
  | NativeReturn     -- Native C-- return
  | Slow             -- Slow entry points: all args pushed on the stack
  | GC               -- Entry to the garbage collector: uses the node reg!
  | PrimOpCall       -- Calling prim ops
  | PrimOpReturn     -- Returning from prim ops
  deriving( Eq )

data ForeignConvention
  = ForeignConvention
        CCallConv               -- Which foreign-call convention
        [ForeignHint]           -- Extra info about the args
        [ForeignHint]           -- Extra info about the result
  deriving Eq

data ForeignTarget        -- The target of a foreign call
  = ForeignTarget                -- A foreign procedure
        CmmExpr                  -- Its address
        ForeignConvention        -- Its calling convention
  | PrimTarget            -- A possibly-side-effecting machine operation
        CallishMachOp            -- Which one
  deriving Eq

data ForeignHint
  = NoHint | AddrHint | SignedHint
  deriving( Eq )
        -- Used to give extra per-argument or per-result
        -- information needed by foreign calling conventions

--------------------------------------------------
-- Instances of register and slot users / definers

instance UserOfLocalRegs (CmmNode e x) where
  foldRegsUsed f z n = case n of
    CmmAssign _ expr -> fold f z expr
    CmmStore addr rval -> fold f (fold f z addr) rval
    CmmUnsafeForeignCall t _ args -> fold f (fold f z t) args
    CmmCondBranch expr _ _ -> fold f z expr
    CmmSwitch expr _ -> fold f z expr
    CmmCall {cml_target=tgt} -> fold f z tgt
    CmmForeignCall {tgt=tgt, args=args} -> fold f (fold f z tgt) args
    _ -> z
    where fold :: forall a b.
                       UserOfLocalRegs a =>
                       (b -> LocalReg -> b) -> b -> a -> b
          fold f z n = foldRegsUsed f z n

instance UserOfLocalRegs ForeignTarget where
  foldRegsUsed _f z (PrimTarget _)      = z
  foldRegsUsed f  z (ForeignTarget e _) = foldRegsUsed f z e

instance DefinerOfLocalRegs (CmmNode e x) where
  foldRegsDefd f z n = case n of
    CmmAssign lhs _ -> fold f z lhs
    CmmUnsafeForeignCall _ fs _ -> fold f z fs
    CmmForeignCall {res=res} -> fold f z res
    _ -> z
    where fold :: forall a b.
                   DefinerOfLocalRegs a =>
                   (b -> LocalReg -> b) -> b -> a -> b
          fold f z n = foldRegsDefd f z n


-----------------------------------
-- mapping Expr in CmmNode

mapForeignTarget :: (CmmExpr -> CmmExpr) -> ForeignTarget -> ForeignTarget 
mapForeignTarget exp   (ForeignTarget e c) = ForeignTarget (exp e) c
mapForeignTarget _   m@(PrimTarget _)      = m

-- Take a transformer on expressions and apply it recursively.
wrapRecExp :: (CmmExpr -> CmmExpr) -> CmmExpr -> CmmExpr
wrapRecExp f (CmmMachOp op es)    = f (CmmMachOp op $ map (wrapRecExp f) es)
wrapRecExp f (CmmLoad addr ty)    = f (CmmLoad (wrapRecExp f addr) ty)
wrapRecExp f e                    = f e

mapExp :: (CmmExpr -> CmmExpr) -> CmmNode e x -> CmmNode e x
mapExp _ f@(CmmEntry _)                          = f
mapExp _ m@(CmmComment _)                        = m
mapExp f   (CmmAssign r e)                       = CmmAssign r (f e)
mapExp f   (CmmStore addr e)                     = CmmStore (f addr) (f e)
mapExp f   (CmmUnsafeForeignCall tgt fs as)      = CmmUnsafeForeignCall (mapForeignTarget f tgt) fs (map f as)
mapExp _ l@(CmmBranch _)                         = l
mapExp f   (CmmCondBranch e ti fi)               = CmmCondBranch (f e) ti fi
mapExp f   (CmmSwitch e tbl)                     = CmmSwitch (f e) tbl
mapExp f   n@CmmCall {cml_target=tgt}            = n{cml_target = f tgt}
mapExp f   (CmmForeignCall tgt fs as succ updfr intrbl) = CmmForeignCall (mapForeignTarget f tgt) fs (map f as) succ updfr intrbl

mapExpDeep :: (CmmExpr -> CmmExpr) -> CmmNode e x -> CmmNode e x
mapExpDeep f = mapExp $ wrapRecExp f

------------------------------------------------------------------------
-- mapping Expr in CmmNode, but not performing allocation if no changes

mapForeignTargetM :: (CmmExpr -> Maybe CmmExpr) -> ForeignTarget -> Maybe ForeignTarget
mapForeignTargetM f (ForeignTarget e c) = (\x -> ForeignTarget x c) `fmap` f e
mapForeignTargetM _ (PrimTarget _)      = Nothing

wrapRecExpM :: (CmmExpr -> Maybe CmmExpr) -> (CmmExpr -> Maybe CmmExpr)
wrapRecExpM f n@(CmmMachOp op es)  = maybe (f n) (f . CmmMachOp op)    (mapListM (wrapRecExpM f) es)
wrapRecExpM f n@(CmmLoad addr ty)  = maybe (f n) (f . flip CmmLoad ty) (wrapRecExpM f addr)
wrapRecExpM f e                    = f e

mapExpM :: (CmmExpr -> Maybe CmmExpr) -> CmmNode e x -> Maybe (CmmNode e x)
mapExpM _ (CmmEntry _)              = Nothing
mapExpM _ (CmmComment _)            = Nothing
mapExpM f (CmmAssign r e)           = CmmAssign r `fmap` f e
mapExpM f (CmmStore addr e)         = (\[addr', e'] -> CmmStore addr' e') `fmap` mapListM f [addr, e]
mapExpM _ (CmmBranch _)             = Nothing
mapExpM f (CmmCondBranch e ti fi)   = (\x -> CmmCondBranch x ti fi) `fmap` f e
mapExpM f (CmmSwitch e tbl)         = (\x -> CmmSwitch x tbl)       `fmap` f e
mapExpM f (CmmCall tgt mb_id r o i s) = (\x -> CmmCall x mb_id r o i s) `fmap` f tgt
mapExpM f (CmmUnsafeForeignCall tgt fs as)
    = case mapForeignTargetM f tgt of
        Just tgt' -> Just (CmmUnsafeForeignCall tgt' fs (mapListJ f as))
        Nothing   -> (\xs -> CmmUnsafeForeignCall tgt fs xs) `fmap` mapListM f as
mapExpM f (CmmForeignCall tgt fs as succ updfr intrbl)
    = case mapForeignTargetM f tgt of
        Just tgt' -> Just (CmmForeignCall tgt' fs (mapListJ f as) succ updfr intrbl)
        Nothing   -> (\xs -> CmmForeignCall tgt fs xs succ updfr intrbl) `fmap` mapListM f as

-- share as much as possible
mapListM :: (a -> Maybe a) -> [a] -> Maybe [a]
mapListM f xs = let (b, r) = mapListT f xs
                in if b then Just r else Nothing

mapListJ :: (a -> Maybe a) -> [a] -> [a]
mapListJ f xs = snd (mapListT f xs)

mapListT :: (a -> Maybe a) -> [a] -> (Bool, [a])
mapListT f xs = foldr g (False, []) (zip3 (tails xs) xs (map f xs))
    where g (_,   y, Nothing) (True, ys)  = (True,  y:ys)
          g (_,   _, Just y)  (True, ys)  = (True,  y:ys)
          g (ys', _, Nothing) (False, _)  = (False, ys')
          g (_,   _, Just y)  (False, ys) = (True,  y:ys)

mapExpDeepM :: (CmmExpr -> Maybe CmmExpr) -> CmmNode e x -> Maybe (CmmNode e x)
mapExpDeepM f = mapExpM $ wrapRecExpM f

-----------------------------------
-- folding Expr in CmmNode

foldExpForeignTarget :: (CmmExpr -> z -> z) -> ForeignTarget -> z -> z 
foldExpForeignTarget exp (ForeignTarget e _) z = exp e z
foldExpForeignTarget _   (PrimTarget _)      z = z

-- Take a folder on expressions and apply it recursively.
wrapRecExpf :: (CmmExpr -> z -> z) -> CmmExpr -> z -> z
wrapRecExpf f e@(CmmMachOp _ es) z = foldr (wrapRecExpf f) (f e z) es
wrapRecExpf f e@(CmmLoad addr _) z = wrapRecExpf f addr (f e z)
wrapRecExpf f e                  z = f e z

foldExp :: (CmmExpr -> z -> z) -> CmmNode e x -> z -> z
foldExp _ (CmmEntry {}) z                         = z
foldExp _ (CmmComment {}) z                       = z
foldExp f (CmmAssign _ e) z                       = f e z
foldExp f (CmmStore addr e) z                     = f addr $ f e z
foldExp f (CmmUnsafeForeignCall t _ as) z         = foldr f (foldExpForeignTarget f t z) as
foldExp _ (CmmBranch _) z                         = z
foldExp f (CmmCondBranch e _ _) z                 = f e z
foldExp f (CmmSwitch e _) z                       = f e z
foldExp f (CmmCall {cml_target=tgt}) z            = f tgt z
foldExp f (CmmForeignCall {tgt=tgt, args=args}) z = foldr f (foldExpForeignTarget f tgt z) args

foldExpDeep :: (CmmExpr -> z -> z) -> CmmNode e x -> z -> z
foldExpDeep f = foldExp go
  where -- go :: CmmExpr -> z -> z
        go e@(CmmMachOp _ es) z = gos es $! f e z
        go e@(CmmLoad addr _) z = go addr $! f e z
        go e                  z = f e z

        gos [] z = z
        gos (e:es) z = gos es $! f e z

-- -----------------------------------------------------------------------------

mapSuccessors :: (Label -> Label) -> CmmNode O C -> CmmNode O C
mapSuccessors f (CmmBranch bid)        = CmmBranch (f bid)
mapSuccessors f (CmmCondBranch p y n)  = CmmCondBranch p (f y) (f n)
mapSuccessors f (CmmSwitch e arms)     = CmmSwitch e (map (fmap f) arms)
mapSuccessors _ n = n


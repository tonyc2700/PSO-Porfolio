{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-incomplete-patterns #-}

module CmmContFlowOpt
    ( cmmCfgOpts
    , cmmCfgOptsProc
    , removeUnreachableBlocks
    , replaceLabels
    )
where

import BlockId
import Cmm
import CmmUtils
import Maybes

import Hoopl
import Control.Monad
import Prelude hiding (succ, unzip, zip)

-----------------------------------------------------------------------------
--
-- Control-flow optimisations
--
-----------------------------------------------------------------------------

cmmCfgOpts :: CmmGraph -> CmmGraph
cmmCfgOpts = removeUnreachableBlocks . blockConcat

cmmCfgOptsProc :: CmmDecl -> CmmDecl
cmmCfgOptsProc = optProc cmmCfgOpts

optProc :: (g -> g) -> GenCmmDecl d h g -> GenCmmDecl d h g
optProc opt (CmmProc info lbl g) = CmmProc info lbl (opt g)
optProc _   top                  = top


-----------------------------------------------------------------------------
--
-- Block concatenation
--
-----------------------------------------------------------------------------

-- This optimisation does two things:
--   - If a block finishes with an unconditional branch, then we may
--     be able to concatenate the block it points to and remove the
--     branch.  We do this either if the destination block is small
--     (e.g. just another branch), or if this is the only jump to
--     this particular destination block.
--
--   - If a block finishes in a call whose continuation block is a
--     goto, then we can shortcut the destination, making the
--     continuation block the destination of the goto.
--
-- Both transformations are improved by working from the end of the
-- graph towards the beginning, because we may be able to perform many
-- shortcuts in one go.


-- We need to walk over the blocks from the end back to the
-- beginning.  We are going to maintain the "current" graph
-- (BlockEnv CmmBlock) as we go, and also a mapping from BlockId
-- to BlockId, representing continuation labels that we have
-- renamed.  This latter mapping is important because we might
-- shortcut a CmmCall continuation.  For example:
--
--    Sp[0] = L
--    call g returns to L
--
--    L: goto M
--
--    M: ...
--
-- So when we shortcut the L block, we need to replace not only
-- the continuation of the call, but also references to L in the
-- code (e.g. the assignment Sp[0] = L).  So we keep track of
-- which labels we have renamed and apply the mapping at the end
-- with replaceLabels.

blockConcat  :: CmmGraph -> CmmGraph
blockConcat g@CmmGraph { g_entry = entry_id }
  = replaceLabels shortcut_map $ ofBlockMap new_entry new_blocks
  where
     -- we might be able to shortcut the entry BlockId itself
     new_entry
       | Just entry_blk <- mapLookup entry_id new_blocks
       , Just dest      <- canShortcut entry_blk
       = dest
       | otherwise
       = entry_id

     blocks = postorderDfs g

     (new_blocks, shortcut_map) =
           foldr maybe_concat (toBlockMap g, mapEmpty) blocks

     maybe_concat :: CmmBlock
                  -> (BlockEnv CmmBlock, BlockEnv BlockId)
                  -> (BlockEnv CmmBlock, BlockEnv BlockId)
     maybe_concat block (blocks, shortcut_map)
        | CmmBranch b' <- last
        , Just blk' <- mapLookup b' blocks
        , shouldConcatWith b' blk'
        = (mapInsert bid (splice head blk') blocks, shortcut_map)

        -- calls: if we can shortcut the continuation label, then
        -- we must *also* remember to substitute for the label in the
        -- code, because we will push it somewhere.
        | Just b'   <- callContinuation_maybe last
        , Just blk' <- mapLookup b' blocks
        , Just dest <- canShortcut blk'
        = (blocks, mapInsert b' dest shortcut_map)
           -- replaceLabels will substitute dest for b' everywhere, later

        -- non-calls: see if we can shortcut any of the successors.
        | Nothing <- callContinuation_maybe last
        = ( mapInsert bid (blockJoinTail head shortcut_last) blocks
          , shortcut_map )

        | otherwise
        = (blocks, shortcut_map)
        where
          (head, last) = blockSplitTail block
          bid = entryLabel block
          shortcut_last = mapSuccessors shortcut last
          shortcut l =
             case mapLookup l blocks of
               Just b | Just dest <- canShortcut b  -> dest
               _otherwise -> l

     shouldConcatWith b block
       | num_preds b == 1    = True  -- only one predecessor: go for it
       | okToDuplicate block = True  -- short enough to duplicate
       | otherwise           = False
       where num_preds bid = mapLookup bid backEdges `orElse` 0

     canShortcut :: CmmBlock -> Maybe BlockId
     canShortcut block
       | (_, middle, CmmBranch dest) <- blockSplit block
       , isEmptyBlock middle
       = Just dest
       | otherwise
       = Nothing

     backEdges :: BlockEnv Int -- number of predecessors for each block
     backEdges = mapInsertWith (+) entry_id 1 $ -- add 1 for the entry id
                   mapMap setSize $ predMap blocks

     splice :: Block CmmNode C O -> CmmBlock -> CmmBlock
     splice head rest = head `blockAppend` snd (blockSplitHead rest)


callContinuation_maybe :: CmmNode O C -> Maybe BlockId
callContinuation_maybe (CmmCall { cml_cont = Just b }) = Just b
callContinuation_maybe (CmmForeignCall { succ = b })   = Just b
callContinuation_maybe _ = Nothing

okToDuplicate :: CmmBlock -> Bool
okToDuplicate block
  = case blockSplit block of
      (_, m, CmmBranch _) -> isEmptyBlock m
      -- cheap and cheerful; we might expand this in the future to
      -- e.g. spot blocks that represent a single instruction or two.
      -- Be careful: a CmmCall can be more than one instruction, it
      -- has a CmmExpr inside it.
      _otherwise -> False

------------------------------------------------------------------------
-- Map over the CmmGraph, replacing each label with its mapping in the
-- supplied BlockEnv.

replaceLabels :: BlockEnv BlockId -> CmmGraph -> CmmGraph
replaceLabels env g
  | mapNull env = g
  | otherwise   = replace_eid $ mapGraphNodes1 txnode g
   where
     replace_eid g = g {g_entry = lookup (g_entry g)}
     lookup id = mapLookup id env `orElse` id

     txnode :: CmmNode e x -> CmmNode e x
     txnode (CmmBranch bid)         = CmmBranch (lookup bid)
     txnode (CmmCondBranch p t f)   = mkCmmCondBranch (exp p) (lookup t) (lookup f)
     txnode (CmmSwitch e arms)      = CmmSwitch (exp e) (map (liftM lookup) arms)
     txnode (CmmCall t k rg a res r) = CmmCall (exp t) (liftM lookup k) rg a res r
     txnode fc@CmmForeignCall{}     = fc{ args = map exp (args fc)
                                        , succ = lookup (succ fc) }
     txnode other                   = mapExpDeep exp other

     exp :: CmmExpr -> CmmExpr
     exp (CmmLit (CmmBlock bid))                = CmmLit (CmmBlock (lookup bid))
     exp (CmmStackSlot (Young id) i) = CmmStackSlot (Young (lookup id)) i
     exp e                                      = e

mkCmmCondBranch :: CmmExpr -> Label -> Label -> CmmNode O C
mkCmmCondBranch p t f = if t == f then CmmBranch t else CmmCondBranch p t f

----------------------------------------------------------------
-- Build a map from a block to its set of predecessors. Very useful.

predMap :: [CmmBlock] -> BlockEnv BlockSet
predMap blocks = foldr add_preds mapEmpty blocks -- find the back edges
  where add_preds block env = foldl (add (entryLabel block)) env (successors block)
        add bid env b' =
          mapInsert b' (setInsert bid (mapLookup b' env `orElse` setEmpty)) env


-----------------------------------------------------------------------------
--
-- Removing unreachable blocks

removeUnreachableBlocks :: CmmGraph -> CmmGraph
removeUnreachableBlocks g
  | length blocks < mapSize (toBlockMap g) = ofBlockList (g_entry g) blocks
  | otherwise = g
  where blocks = postorderDfs g

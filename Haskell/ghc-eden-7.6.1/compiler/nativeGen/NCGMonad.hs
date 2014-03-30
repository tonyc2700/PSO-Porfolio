-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 1993-2004
-- 
-- The native code generator's monad.
--
-- -----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module NCGMonad (
	NatM_State(..), mkNatM_State,

	NatM, -- instance Monad
	initNat, 
	addImportNat, 
	getUniqueNat,
	mapAccumLNat, 
	setDeltaNat, 
	getDeltaNat,
	getBlockIdNat, 
	getNewLabelNat, 
	getNewRegNat, 
	getNewRegPairNat,
	getPicBaseMaybeNat, 
	getPicBaseNat, 
	getDynFlags
) 
 
where
  
#include "HsVersions.h"

import Reg
import Size
import TargetReg

import BlockId
import CLabel		( CLabel, mkAsmTempLabel )
import UniqSupply
import Unique		( Unique )
import DynFlags

data NatM_State 
	= NatM_State {
		natm_us      :: UniqSupply,
		natm_delta   :: Int,
		natm_imports :: [(CLabel)],
		natm_pic     :: Maybe Reg,
		natm_dflags  :: DynFlags
	}

newtype NatM result = NatM (NatM_State -> (result, NatM_State))

unNat :: NatM a -> NatM_State -> (a, NatM_State)
unNat (NatM a) = a

mkNatM_State :: UniqSupply -> Int -> DynFlags -> NatM_State
mkNatM_State us delta dflags 
	= NatM_State us delta [] Nothing dflags

initNat :: NatM_State -> NatM a -> (a, NatM_State)
initNat init_st m 
	= case unNat m init_st of { (r,st) -> (r,st) }


instance Monad NatM where
  (>>=) = thenNat
  return = returnNat


thenNat :: NatM a -> (a -> NatM b) -> NatM b
thenNat expr cont
	= NatM $ \st -> case unNat expr st of
			(result, st') -> unNat (cont result) st'

returnNat :: a -> NatM a
returnNat result 
	= NatM $ \st ->  (result, st)

mapAccumLNat :: (acc -> x -> NatM (acc, y))
                -> acc
	        -> [x]
	        -> NatM (acc, [y])

mapAccumLNat _ b []
  = return (b, [])
mapAccumLNat f b (x:xs)
  = do (b__2, x__2)  <- f b x
       (b__3, xs__2) <- mapAccumLNat f b__2 xs
       return (b__3, x__2:xs__2)

getUniqueNat :: NatM Unique
getUniqueNat = NatM $ \ (NatM_State us delta imports pic dflags) ->
    case takeUniqFromSupply us of
         (uniq, us') -> (uniq, (NatM_State us' delta imports pic dflags))

instance HasDynFlags NatM where
    getDynFlags = NatM $ \ (NatM_State us delta imports pic dflags) ->
                             (dflags, (NatM_State us delta imports pic dflags))


getDeltaNat :: NatM Int
getDeltaNat 
	= NatM $ \ st -> (natm_delta st, st)


setDeltaNat :: Int -> NatM ()
setDeltaNat delta 
	= NatM $ \ (NatM_State us _ imports pic dflags) ->
		   ((), NatM_State us delta imports pic dflags)


addImportNat :: CLabel -> NatM ()
addImportNat imp 
	= NatM $ \ (NatM_State us delta imports pic dflags) ->
		   ((), NatM_State us delta (imp:imports) pic dflags)


getBlockIdNat :: NatM BlockId
getBlockIdNat 
 = do	u <- getUniqueNat
 	return (mkBlockId u)


getNewLabelNat :: NatM CLabel
getNewLabelNat 
 = do 	u <- getUniqueNat
 	return (mkAsmTempLabel u)


getNewRegNat :: Size -> NatM Reg
getNewRegNat rep
 = do u <- getUniqueNat
      dflags <- getDynFlags
      return (RegVirtual $ targetMkVirtualReg (targetPlatform dflags) u rep)


getNewRegPairNat :: Size -> NatM (Reg,Reg)
getNewRegPairNat rep
 = do u <- getUniqueNat
      dflags <- getDynFlags
      let vLo = targetMkVirtualReg (targetPlatform dflags) u rep
      let lo  = RegVirtual $ targetMkVirtualReg (targetPlatform dflags) u rep
      let hi  = RegVirtual $ getHiVirtualRegFromLo vLo
      return (lo, hi)


getPicBaseMaybeNat :: NatM (Maybe Reg)
getPicBaseMaybeNat 
	= NatM (\state -> (natm_pic state, state))


getPicBaseNat :: Size -> NatM Reg
getPicBaseNat rep 
 = do	mbPicBase <- getPicBaseMaybeNat
	case mbPicBase of
	        Just picBase -> return picBase
	        Nothing 
		 -> do
			reg <- getNewRegNat rep
			NatM (\state -> (reg, state { natm_pic = Just reg }))

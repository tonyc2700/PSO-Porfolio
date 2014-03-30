-----------------------------------------------------------------------------
--
-- Pretty-printing assembly language
--
-- (c) The University of Glasgow 1993-2005
--
-----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module PprBase (
	castFloatToWord8Array,
	castDoubleToWord8Array,
	floatToBytes,
	doubleToBytes
)

where

-- castSTUArray has moved to Data.Array.Unsafe
#if __GLASGOW_HASKELL__ >= 703
import Data.Array.Unsafe( castSTUArray )
import Data.Array.ST hiding( castSTUArray )
#else
import Data.Array.ST
#endif

import Control.Monad.ST

import Data.Word



-- -----------------------------------------------------------------------------
-- Converting floating-point literals to integrals for printing

castFloatToWord8Array :: STUArray s Int Float -> ST s (STUArray s Int Word8)
castFloatToWord8Array = castSTUArray

castDoubleToWord8Array :: STUArray s Int Double -> ST s (STUArray s Int Word8)
castDoubleToWord8Array = castSTUArray

-- floatToBytes and doubleToBytes convert to the host's byte
-- order.  Providing that we're not cross-compiling for a 
-- target with the opposite endianness, this should work ok
-- on all targets.

-- ToDo: this stuff is very similar to the shenanigans in PprAbs,
-- could they be merged?

floatToBytes :: Float -> [Int]
floatToBytes f
   = runST (do
        arr <- newArray_ ((0::Int),3)
        writeArray arr 0 f
        arr <- castFloatToWord8Array arr
        i0 <- readArray arr 0
        i1 <- readArray arr 1
        i2 <- readArray arr 2
        i3 <- readArray arr 3
        return (map fromIntegral [i0,i1,i2,i3])
     )

doubleToBytes :: Double -> [Int]
doubleToBytes d
   = runST (do
        arr <- newArray_ ((0::Int),7)
        writeArray arr 0 d
        arr <- castDoubleToWord8Array arr
        i0 <- readArray arr 0
        i1 <- readArray arr 1
        i2 <- readArray arr 2
        i3 <- readArray arr 3
        i4 <- readArray arr 4
        i5 <- readArray arr 5
        i6 <- readArray arr 6
        i7 <- readArray arr 7
        return (map fromIntegral [i0,i1,i2,i3,i4,i5,i6,i7])
     )


module FilenameDescr where

import Data.Either
import Data.List

import BuildInfo
import Utils
import Tar

-- We can't just compare plain filenames, because versions numbers of GHC
-- and the libaries will vary. So we use FilenameDescr instead, which
-- abstracts out the version numbers.
type FilenameDescr = [FilenameDescrBit]
data FilenameDescrBit = VersionOf String
                      | HashOf String
                      | FP String
                      | Ways
    deriving (Show, Eq, Ord)

normalise :: FilenameDescr -> FilenameDescr
normalise [] = []
normalise [x] = [x]
normalise (FP x1 : FP x2 : xs) = normalise (FP (x1 ++ x2) : xs)
normalise (x : xs) = x : normalise xs

-- Sanity check that the FilenameDescr matches the filename in the tar line
checkContent :: BuildInfo -> (FilenameDescr, TarLine) -> Errors
checkContent buildInfo (fd, tl)
 = let fn = tlFileName tl
   in case flattenFilenameDescr buildInfo fd of
      Right fn' ->
          if fn' == fn
          then []
          else ["checkContent: Can't happen: filename mismatch: " ++ show fn]
      Left errs ->
          errs

flattenFilenameDescr :: BuildInfo -> FilenameDescr
                     -> Either Errors FilePath
flattenFilenameDescr buildInfo fd = case partitionEithers (map f fd) of
                                    ([], strs) -> Right (concat strs)
                                    (errs, _) -> Left (concat errs)
    where f (FP fp) = Right fp
          f (VersionOf thing)
           = case lookup thing (biThingVersionMap buildInfo) of
             Just v -> Right v
             Nothing -> Left ["Can't happen: thing has no version in mapping"]
          f (HashOf thing)
           = case lookup thing (biThingHashMap buildInfo) of
             Just v -> Right v
             Nothing -> Left ["Can't happen: thing has no hash in mapping"]
          f Ways = case biMaybeWays buildInfo of
                   Just ways -> Right $ intercalate "-" ways
                   Nothing   -> Left ["Can't happen: No ways, but Ways is used"]


module FileOperations where

import Prelude
import Control.MonadZero (guard)
import Data.Array (concatMap, (:), head, tail)
import Data.Foldable (foldl, minimumBy)
import Data.Maybe (Maybe(..), fromJust)
import Data.Path (Path, isDirectory, ls, root, size)
import Partial.Unsafe (unsafePartial)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child


onlyFiles :: Path -> Array Path
onlyFiles file = do
  child <- ls file
  guard $ isDirectory child
  grandChild <- ls child
  pure grandChild

unsafeSize :: Path -> Int
unsafeSize a = unsafePartial $ fromJust $ size a

selectFileWith :: (Int -> Int -> Boolean) -> Maybe Path
selectFileWith comparator = minimumBy fileCompare (onlyFiles root) where
  fileCompare a b =  if comparator (unsafeSize a) (unsafeSize b) then LT else GT

largestFile :: Maybe Path
largestFile = selectFileWith (>)

smallestFile :: Maybe Path
smallestFile = selectFileWith (<)

-- split file path into file name and directory
-- get file directory
-- WhereIs :: Path -> Maybe Path

module Chapter8 where
import Data.List (List(..), head, tail, foldM)
import Data.Maybe(Maybe(..))
import Prelude

third :: forall a. List a -> Maybe a
third xs = do
  ys <- tail xs
  zs <- tail ys
  head zs

total :: forall a. List a -> List a
total  = nub <<< sort <<< foldM(\x y -> pure (x + y)) 0

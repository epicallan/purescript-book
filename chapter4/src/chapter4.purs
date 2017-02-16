module Chapter4 where
import Prelude
import Data.Array (null, (..))
import Data.List (List(..), filter, (:))
import Data.Array.Partial (tail)
import Data.Tuple(Tuple(..))
import Partial.Unsafe (unsafePartial)
import Control.MonadZero (guard)
import Data.Foldable(foldl)


length :: forall a. Array a -> Int
length arr =
  if null arr
    then 0
    else 1 + length (unsafePartial tail arr)

even :: Int -> Boolean
even n =
  if n < 0
  then even (-n)
  else if n == 0
       then true
       else if n == 1
            then false
            else even (n-2)

evenCount :: List Int -> Int
evenCount Nil = 0
evenCount (Cons x xs)
    | even x == true = 1 + evenCount xs
    | even x == false =  0 + evenCount xs

evenCount (Cons _ xs) = evenCount xs

-- infix 9 filter as <$?>
removeNegatives :: List Int -> List Int
removeNegatives = filter (\n -> n >= 0)

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]


cartProd :: Array Int -> Array Int -> Array (Tuple Int Int)
cartProd xs ys = do
  i <- xs
  j <- ys
  [Tuple i j]

pythogoreanTriple :: Int -> Array (Array Int)
pythogoreanTriple n = do
  a <- 1 .. n
  b <- 1 .. n
  c <- 1 .. n
  guard $ a * a + b * b == c * c
  pure [a,b,c]

boolAllTrue :: Array Boolean -> Boolean
boolAllTrue = foldl(\x acc -> x && acc ) true

count :: (Int -> Boolean) -> List Int -> Int
count p list = count' p list 0 where
  count' _ Nil acc    = acc
  count' f (x:xs) acc = if f x
                        then count' f xs (acc+1)
                        else count' f xs acc

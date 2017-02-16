module Chapter6 where
import Data.List (List(..), (:))
import Prelude
import Data.Foldable
import Data.Monoid


-- exercise
newtype Complex = Complex
 { real :: Number
 , imaginary :: Number
 }

instance complexShow :: Show Complex where
  show (Complex {real, imaginary}) = "Real: "<> (show real) <> " imaginary: " <> (show imaginary)

data NonEmpty a = NonEmpty a (List a)

instance nonEmptyEq :: (Eq a) => Eq (NonEmpty a) where
  eq (NonEmpty x xs) (NonEmpty y ys) = (x : xs) == (y : ys)
  -- notEq (NonEmpty x xs) (NonEmpty y ys) = (x : xs) /= (y : ys)

instance nonEmptySemigroup :: (Semigroup a) => Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (xs <> Cons x Nil <> ys)

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty x xs) = NonEmpty (f x) (map f xs)

instance foldableNonEmpty :: Foldable NonEmpty where
  foldr f z (NonEmpty x xs) = foldr f z (x : xs)
  foldl f z (NonEmpty x xs) = foldl f z (x : xs)
  foldMap f (NonEmpty x xs) = foldMap f (x : xs)

class Monoid m <= Action m a where
   act :: m -> a -> a

instance listAction :: (Monoid m, List a, Action m a) => Action m a where
  act x y = map (act x) y

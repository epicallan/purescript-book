module Data.AddressBook.Validation where

import Prelude
import Control.Alt (class Functor)
import Data.AddressBook (Address(..), Person(..), PhoneNumber(..), address, person, phoneNumber)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.String (length)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (class Traversable, traverse)
import Data.Validation.Semigroup (V, unV, invalid)
import Partial.Unsafe (unsafePartial)


type Errors = Array String

nonEmpty :: String -> String -> V Errors Unit
nonEmpty field "" = invalid ["Field '" <> field <> "' cannot be empty"]
nonEmpty _     _  = pure unit

arrayNonEmpty :: forall a. String -> Array a -> V Errors Unit
arrayNonEmpty field [] = invalid ["Field '" <> field <> "' must contain at least one value"]
arrayNonEmpty _     _  = pure unit

lengthIs :: String -> Int -> String -> V Errors Unit
lengthIs field len value | length value /= len = invalid ["Field '" <> field <> "' must have length " <> show len]
lengthIs _     _   _     = pure unit

phoneNumberRegex :: Regex
phoneNumberRegex =
  unsafePartial
    case regex "^\\d{3}-\\d{3}-\\d{4}$" noFlags of
      Right r -> r

matches :: String -> Regex -> String -> V Errors Unit
matches _     regex value | test regex value = pure unit
matches field _     _     = invalid ["Field '" <> field <> "' did not match the required format"]

validateAddress :: Address -> V Errors Address
validateAddress (Address o) =
  address <$> (nonEmpty "Street" o.street *> pure o.street)
          <*> (nonEmpty "City"   o.city   *> pure o.city)
          <*> (lengthIs "State" 2 o.state *> pure o.state)

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber (PhoneNumber o) =
  phoneNumber <$> pure o."type"
              <*> (matches "Number" phoneNumberRegex o.number *> pure o.number)

validatePerson :: Person -> V Errors Person
validatePerson (Person o) =
  person <$> (nonEmpty "First Name" o.firstName *> pure o.firstName)
         <*> (nonEmpty "Last Name"  o.lastName  *> pure o.lastName)
         <*> validateAddress o.homeAddress
         <*> (arrayNonEmpty "Phone Numbers" o.phones *> traverse validatePhoneNumber o.phones)

validatePerson' :: Person -> Either Errors Person
validatePerson' p = unV Left Right $ validatePerson p


data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)

instance treeFunctor  :: Functor Tree where
  map _ Empty = Empty
  map f (Leaf x) = Leaf (f x)
  map f (Node l x r) = Node (map f l) (f x) (map f r)


instance foldableTree :: Foldable Tree where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr _ z Empty = z
  foldr f z (Leaf a) = (f z a)
  foldr f z (Node l x r) = foldr f (f z (foldr z l)) r

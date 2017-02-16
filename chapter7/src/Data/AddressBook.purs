module Data.AddressBook where
import Prelude
import Data.Either
import Data.Maybe
import Control.Apply(lift2, (<$>), (<*>))

newtype Address = Address
  { street :: String
  , city   :: String
  , state  :: String
  }

address :: String -> String -> String -> Address
address street city state = Address { street, city, state }

data PhoneType
  = HomePhone
  | WorkPhone
  | CellPhone
  | OtherPhone

newtype PhoneNumber = PhoneNumber
  { "type" :: PhoneType
  , number :: String
  }

phoneNumber :: PhoneType -> String -> PhoneNumber
phoneNumber ty number = PhoneNumber
  { "type": ty
  , number: number
  }

newtype Person = Person
  { firstName   :: String
  , lastName    :: String
  , homeAddress :: Address
  , phones      :: Array PhoneNumber
  }

person :: String -> String -> Address -> Array PhoneNumber -> Person
person firstName lastName homeAddress phones =
  Person { firstName, lastName, homeAddress, phones }

examplePerson :: Person
examplePerson =
  person "John" "Smith"
         (address "123 Fake St." "FakeTown" "CA")
         [ phoneNumber HomePhone "555-555-5555"
         , phoneNumber CellPhone "555-555-0000"
         ]

instance showAddress :: Show Address where
  show (Address o) = "Address " <>
    "{ street: " <> show o.street <>
    ", city: "   <> show o.city <>
    ", state: "  <> show o.state <>
    " }"

instance showPhoneType :: Show PhoneType where
  show HomePhone = "HomePhone"
  show WorkPhone = "WorkPhone"
  show CellPhone = "CellPhone"
  show OtherPhone = "OtherPhone"

instance showPhoneNumber :: Show PhoneNumber where
  show (PhoneNumber o) = "PhoneNumber " <>
    "{ type: "   <> show o."type" <>
    ", number: " <> show o.number <>
    " }"

instance showPerson :: Show Person where
  show (Person o) = "Person " <>
    "{ firstName: "   <> show o.firstName <>
    ", lastName: "    <> show o.lastName <>
    ", homeAddress: " <> show o.homeAddress <>
    ", phones: "      <> show o.phones <>
    " }"

--- exercises


maybeAdd :: Maybe Number -> Maybe Number -> Maybe Number
maybeAdd = lift2 (+)

-- lift3' :: forall a b c d f. Apply f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
-- lift3' f a b c = f <$> a <*> b <$> b <*> c

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just x ) = Just <$> x


nonEmpty :: String -> Either String Unit
nonEmpty "" = Left "Field cannot be empty"
nonEmpty _  = Right unit

validatePerson :: Person -> Either String Person
validatePerson (Person o) =
  person <$> (nonEmpty o.firstName *> pure o.firstName)
         <*> (nonEmpty o.lastName  *> pure o.lastName)
         <*> pure o.homeAddress
         <*> pure o.phones

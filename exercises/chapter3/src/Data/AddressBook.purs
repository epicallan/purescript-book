module Data.AddressBook where

import Prelude
import Control.Plus (empty)
import Data.BooleanAlgebra (class BooleanAlgebra)
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet streetName = head <<< filter filterForStreet
  where
  filterForStreet :: Entry -> Boolean
  filterForStreet entry = entry.address.street == streetName

print :: Maybe Entry -> Maybe String
print entry = map showEntry entry

printByStreet :: String -> AddressBook -> Maybe String
printByStreet streetName book = print $ findEntryByStreet streetName book

isNameInAddressBook :: String -> AddressBook -> Boolean
isNameInAddressBook firstName = null <<< filter filterByFirstName
  where
  filterByFirstName :: Entry -> Boolean
  filterByFirstName entry = entry.firstName == firstName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy (\x y -> x.firstName == y.firstName && x.lastName == y.lastName)

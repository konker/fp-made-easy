module Ch6 where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Newtype (class Newtype)

data Address = Address
  { street1 :: String
  , street2 :: String
  , city :: String
  , state :: String
  , zip :: String
  }

class HasAddress a where
  getAddress :: a -> Address

instance eqAddress :: Eq Address where
  eq (Address a1) (Address a2) = a1 == a2

data Company = Company
  { name :: String
  , address :: Address
  }

data Residence
  = Home Address
  | Facility Address

newtype Person = Person
  { name :: String
  , age :: Int
  , address :: Address
  }

instance eqPerson :: Eq Person where
  eq (Person p1) (Person p2) = p1 == p2

instance hasAddressPerson :: HasAddress Person where
  getAddress (Person p) = p.address

{-
genericPersonHasAddress :: ∀ a. Newtype a Person => a -> Address
genericPersonHasAddress wrappedPerson =
  getAddress $ unwrap  wrappedPerson
-}

newtype Ceo = Ceo Person
derive instance newtypeCeo :: Newtype Ceo _
derive newtype instance hasAddressCeo :: HasAddress Ceo

{-
instance hasAddressCeo :: HasAddress Ceo where
  --getAddress (Ceo  p) = getAddress p
  getAddress = genericPersonHasAddress
-}

newtype Janitor = Janitor Person
derive instance newtypeJanitor :: Newtype Janitor _
derive newtype instance hasAddressJanitor :: HasAddress Janitor

{-
instance hasAddressJanitor :: HasAddress Janitor where
  --getAddress (Janitor  p) = getAddress p
  getAddress = genericPersonHasAddress
-}

------
data SomeType = This | That | TheOther | AndYetAnother

derive instance eqSomeType :: Eq SomeType
derive instance ordSomeType :: Ord SomeType
derive instance genericSomeType :: Generic SomeType _
instance showSomeType :: Show SomeType where
  show = genericShow

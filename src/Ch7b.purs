module Ch7b where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)

------------------------------------------------------------------------------
-- Types
newtype FullName
  = FullName String

derive instance newtypeFullName :: Newtype FullName _
derive newtype instance fullNameEq :: Eq FullName
-- derive newtype instance fullNameShow :: Show FullName
instance fullNameShow :: Show FullName where
  show (FullName name) = name

------------------------------------------------------------------------------
newtype Age
  = Age Int

derive instance newtypeAge :: Newtype Age _
derive newtype instance eqAge :: Eq Age
derive newtype instance showAge :: Show Age

------------------------------------------------------------------------------
data Occupation
  = Doctor
  | Dentist
  | Lawyer
  | Unemployed

derive instance genericOccupation :: Generic Occupation _
derive instance occupationEq :: Eq Occupation

instance occupationShow :: Show Occupation where
  show = genericShow

fromStringOccupation :: String -> Maybe Occupation
fromStringOccupation s = case s of
  "Doctor" -> Just Doctor
  "Dentist" -> Just Dentist
  "Lawyer" -> Just Lawyer
  "Unemployed" -> Just Unemployed
  _ -> Nothing

------------------------------------------------------------------------------
data Person
  = Person
  { name :: FullName
  , age :: Age
  , occupation :: Occupation
  }

derive instance personEq :: Eq Person
instance personShow :: Show Person where
  show (Person p) = "Person: {" <> show p.name <> "," <> show p.age <> "," <> show p.occupation <> "}"

------------------------------------------------------------------------------
-- CSV
newtype CSV
  = CSV String

derive instance newtypeCSV :: Newtype CSV _
derive newtype instance eqCSV :: Eq CSV
derive newtype instance showCSV :: Show CSV

------------------------------------------------------------------------------
-- ToCSV
class ToCSV a where
  toCSV :: a -> CSV

instance toCsvPerson :: ToCSV Person where
  toCSV (Person p) =
    CSV $ show p.name <> "," <> show p.age <> "," <> show p.occupation

------------------------------------------------------------------------------
-- FromCSV
class FromCSV a where
  fromCSV :: CSV -> Maybe a

instance fromCsvPerson :: FromCSV Person where
  fromCSV (CSV row) = case split (Pattern ",") row of
    [ name, age, occupation ] -> case fromString age of
      Just age' -> case fromStringOccupation occupation of
        Just occupation' ->
          Just
            $ Person
                { name: FullName name
                , age: Age age'
                , occupation: occupation'
                }
        Nothing -> Nothing
      Nothing -> Nothing
    _ -> Nothing

------------------------------------------------------------------------------
-- Engage
test :: Effect Unit
test = do
  log
    $ show
    $
      toCSV
        ( Person
            { name: FullName "Sue Smith"
            , age: Age 23
            , occupation: Doctor
            }
        )
        == CSV "Sue Smith,23,Doctor"
  let
    person =
      Person
        { name: FullName "Sue Smith"
        , age: Age 23
        , occupation: Doctor
        }
  log $ show $ ((toCSV person # fromCSV) :: Maybe Person) == Just person

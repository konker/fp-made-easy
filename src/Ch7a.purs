module Ch7a where

import Prelude (Unit, show, discard, otherwise, (==), ($), (<), (>), (<=), (||))

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Ord (class Ord, Ordering(..), compare)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

data Either a b
  = Left a
  | Right b

derive instance genericEither :: Generic (Either a b) _
derive instance eitherEq :: (Eq a, Eq b) => Eq (Either a b)
derive instance eitherOrd :: (Ord a, Ord b) => Ord (Either a b)
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

data Maybe a
  = Nothing
  | Just a

derive instance genericMaybe :: Generic (Maybe a) _
derive instance maybeEq :: Eq a => Eq (Maybe a)
derive instance maybeOrd :: Ord a => Ord (Maybe a)
instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

{-
instance maybeEq :: Eq a => Eq (Maybe a) where
  eq Nothing Nothing = true
  eq (Just x) (Just y) = x == y
  eq _ _ = false
instance maybeOrd :: Ord a => Ord (Maybe a) where
  compare Nothing Nothing = EQ
  compare Nothing (Just _) = LT
  compare (Just _) Nothing = GT
  compare (Just x) (Just y) = compare x y
instance maybeShow :: Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just x) = "Just(" <> show x <> ")"
-}

greaterThanOrEq' :: ∀ a. Ord a => a -> a -> Boolean
greaterThanOrEq' x y
  | compare x y == GT = true
  | compare x y == EQ = true
  | otherwise = false

greaterThanOrEq :: ∀ a. Ord a => a -> a -> Boolean
greaterThanOrEq x y = cmp == GT || cmp == EQ
  where
  cmp = compare x y

infixl 4 greaterThanOrEq as >=

------------------------------------------------------------------------------
--- Engage
test :: Effect Unit
test = do
  log "--- Eq"
  log $ show $ Just 5 == Just 5
  log $ show $ Just 5 == Just 2
  log $ show $ Just 5 == Nothing
  log $ show $ Nothing == Just 5
  log $ show $ Nothing == (Nothing :: Maybe Unit)
  log "--- Ord"
  log $ show $ Just 1 < Just 5
  log $ show $ Just 5 <= Just 5
  log $ show $ Just 5 > Just 10
  log $ show $ Just 10 >= Just 10
  log $ show $ Just 99 > Nothing
  log $ show $ Just 99 < Nothing
  log "--- Show"
  log $ show $ Just "abc"
  log $ show $ (Nothing :: Maybe Unit)
  log $ show $ (Left "left" :: Either _ Unit)
  log $ show $ (Right (Just 42) :: Either Unit _)


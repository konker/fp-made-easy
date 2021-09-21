module Ch13 where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Data.String (toUpper)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, identity, show, ($), (*), (+), (/), (<<<), (<>), (==))

-------------------------------------------------------------------------
class Functor f where
  map :: ∀ a b. (a -> b) -> f a -> f b

infixl 4 map as <$>

class Bifunctor f where
  bimap :: ∀ a b c d. (a -> b) -> (c -> d) -> f a c -> f b d

lmap :: ∀ f a b c. Bifunctor f => (a -> b) -> f a c -> f b c
lmap fun = bimap fun identity

rmap :: ∀ f a b c. Bifunctor f => (b -> c) -> f a b -> f a c
rmap fun = bimap identity fun

-------------------------------------------------------------------------
data Maybe a = Nothing | Just a

derive instance eqMaybe :: Eq a => Eq (Maybe a)
derive instance genericMaybe :: Generic (Maybe a) _
instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

instance maybeFunctor :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

-------------------------------------------------------------------------
data Either a b = Left a | Right b

derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

instance functorEither :: Functor (Either a) where
  map _ (Left x) = Left x
  map f (Right y) = Right $ f y

instance bifunctorEither :: Bifunctor Either where
  bimap f _ (Left x) = Left (f x)
  bimap _ g (Right x) = Right (g x)

-------------------------------------------------------------------------
data Tuple a b = Tuple a b

derive instance eqTuple :: (Eq a, Eq b) => Eq (Tuple a b)
derive instance genericTuple :: Generic (Tuple a b) _
instance tupleShow :: (Show a, Show b) => Show (Tuple a b) where
  show = genericShow

instance functorTuple :: Functor (Tuple a) where
  map f (Tuple x y) = Tuple x (f y)

instance bifunctorTuple :: Bifunctor Tuple where
  bimap f g (Tuple x y) = Tuple (f x) (g y)

-------------------------------------------------------------------------
data Threeple a b c = Threeple a b c

derive instance genericThreeple :: Generic (Threeple a b c) _
instance showThreeple :: (Show a, Show b, Show c) => Show (Threeple a b c) where
  show = genericShow

instance functorThreeple :: Functor (Threeple a b) where
  map f (Threeple x y z) = Threeple x y (f z)

instance bifunctorThreeple :: Bifunctor (Threeple a) where
  bimap f g (Threeple x y z) = Threeple x (f y) (g z)

-------------------------------------------------------------------------
-- Engage
test :: Effect Unit
test = do
  log "--- Maybe"
  log $ show $ (_ / 2) <$> Just 10
  log $ show $ (_ / 2) <$> Nothing
  log $ show $ "Maybe Identity for Nothing: "
    <> show ((identity <$> Nothing) == (Nothing :: Maybe Unit))
  log $ show $ "Maybe Identity for Just: "
    <> show ((identity <$> Just 10) == Just 10)
  let
    g x = x / 2
    f x = x + 2
  log $ show $ "Composition law for Nothing: "
    <> show ((map (g <<< f) Nothing) == ((map g) <<< (map f)) Nothing)
  log $ show $ "Composition law for Just: "
    <> show ((map (g <<< f) (Just 10)) == ((map g) <<< (map f)) (Just 10))
  log "--- Either"
  log $ show $ (_ / 2) <$> (Right 10 :: Either Unit _)
  log $ show $ (_ / 2) <$> Left "error reason"
  log $ show $ rmap (_ * 2) $ Left "error reason"
  log $ show $ rmap (_ * 2) $ (Right 10 :: Either Unit _)
  log $ show $ lmap toUpper $ (Left "error reason" :: Either _ Unit)
  log $ show $ lmap toUpper $ Right 10
  log "--- Tuple"
  log $ show $ (_ / 2) <$> Tuple 10 20
  log $ show $ "Tuple Identity: "
    <> show ((identity <$> Tuple 10 20) == Tuple 10 20)
  log $ show $ "Tuple Composition : "
    <> show ((map (g <<< f) (Tuple 10 20)) == (map g <<< map f) (Tuple 10 20))
  log $ show $ rmap (_ * 2) $ Tuple 80 40
  log $ show $ lmap (_ / 2) $ Tuple 80 40
  log $ show $ bimap (_ / 2) (_ * 2) $ Tuple 80 40
  log "--- Threeple"
  log $ show $ (_ / 2) <$> Threeple 10 20 40
  log $ show $ rmap (_ * 2) $ Threeple 99 80 40
  log $ show $ lmap (_ / 2) $ Threeple 99 80 40
  log $ show $ bimap (_ / 2) (_ * 2) $ Threeple 99 80 40

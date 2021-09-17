module Ch11 where

import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.List (List(..), singleton, (:))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..), (:|))
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Ord, class Semiring, type (~>), Unit, discard, negate, otherwise, show, zero, ($), (+), (>=), (<>), (<<<))

reverse :: List ~> List
reverse = foldl (\acc x -> x : acc) Nil

foldl1 :: ∀ f a. Foldable f => (a -> a -> a) -> NonEmpty f a -> a
foldl1 fun (x :| xs) = foldl fun x xs

max :: ∀ a. Ord a => a -> a -> a
max x y
  | x >= y = x
  | otherwise = y

-------------------------------------------------------------------------
findMax' :: ∀ a. Ord a => a -> List a -> a
findMax' acc Nil = acc
findMax' acc (x : xs) = findMax' (max acc x) xs

findMax'' :: ∀ a. Ord a => List a -> Maybe a
findMax'' Nil = Nothing
findMax'' (x : xs) = Just $ go x xs
  where
  go acc Nil = acc
  go acc (x' : xs') = go (max acc x') xs'

findMax :: ∀ a. Ord a => List a -> Maybe a
findMax Nil = Nothing
findMax l@(x : _) = Just $ foldl max x l

findMaxNE' :: ∀ a. Ord a => NonEmptyList a -> a
findMaxNE' (NonEmptyList (NonEmpty x xs)) = foldl max x xs

findMaxNE :: ∀ a. Ord a => NonEmptyList a -> a
findMaxNE (NonEmptyList l) = foldl1 max l

-------------------------------------------------------------------------
sum' :: List Int -> Int
sum' = go 0
  where
  go acc Nil = acc
  go acc (x : xs) = go (acc + x) xs

sum'' :: List Int -> Int
sum'' = foldl (+) 0

sum :: ∀ f a. Foldable f => Semiring a => f a -> a
sum = foldl (+) zero

-------------------------------------------------------------------------
data Tree a = Leaf a | Node (Tree a) (Tree a)

toList :: ∀ a. Tree a -> List a
toList (Leaf x) = singleton x
toList (Node l r) = toList l <> toList r

instance foldableTree :: Foldable Tree where
  foldr f b = foldr f b <<< toList
  foldl f b = foldl f b <<< toList
  foldMap f = foldMap f <<< toList

-------------------------------------------------------------------------
-- Engage
test :: Effect Unit
test = do
  log "--- reverse"
  log $ show $ reverse (10 : 20 : 30 : Nil)
  log "--- max"
  log $ show $ max (-1) 99
  log $ show $ max "aa" "z"
  log "--- findMax"
  log $ show $ findMax' 0 (37 : 311 : -1 : 2 : 84 : Nil)
  log $ show $ findMax' "" ("a" : "bbb" : "c" : Nil)
  log $ show $ findMax'' (37 : 311 : -1 : 2 : 84 : Nil)
  log $ show $ findMax'' ("a" : "bbb" : "c" : Nil)
  log $ show $ findMax'' (Nil :: List Unit)
  log $ show $ findMax (37 : 311 : -1 : 2 : 84 : Nil)
  log $ show $ findMax ("a" : "bbb" : "c" : Nil)
  log $ show $ findMax (Nil :: List Unit)
  log "--- findMaxNE"
  log $ show $ findMaxNE' (NonEmptyList $ 37 :| (311 : -1 : 2 : 84 : Nil))
  log $ show $ findMaxNE' (NonEmptyList $ "a" :| ("bbb" : "c" : Nil))
  log $ show $ findMaxNE (NonEmptyList $ 37 :| (311 : -1 : 2 : 84 : Nil))
  log $ show $ findMaxNE (NonEmptyList $ "a" :| ("bbb" : "c" : Nil))
  log "--- sum"
  log $ show $ sum' (1 : 2 : 3 : Nil)
  log $ show $ sum'' (1 : 2 : 3 : Nil)
  log $ show $ sum (1 : 2 : 3 : Nil)
  log $ show $ sum (1.1 : 2.2 : 3.3 : Nil)
  log "-- tree"
  log $ show $ toList
    (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))
  log $ show $ sum
    (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))

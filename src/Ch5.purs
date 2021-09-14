module Ch5 where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), Unit, discard, max, negate, otherwise, show, (+), (-), (/=), (<), (<<<), (>>>), (==), (>), (>=))

const :: ∀ a b. a -> b -> a
const x _ = x

apply :: ∀ a b. (a -> b) -> a -> b
apply f x = f x

infixr 0 apply as $

flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip f x y = f y x

applyFlipped :: ∀ a b. a -> (a -> b) -> b
--applyFlipped x f = f x
applyFlipped = flip apply

infixl 1 applyFlipped as #

singleton :: ∀ a. a -> List a
singleton x = x : Nil

null :: ∀ a. List a -> Boolean
null Nil = true
null _ = false

snoc :: ∀ a. List a -> a -> List a
snoc Nil x = singleton x
snoc (y : ys) x = y : snoc ys x

length' :: ∀ a. List a -> Int
length' Nil = 0
length' (_ : xs) = 1 + length xs

-- tail recursive
length :: ∀ a. List a -> Int
length l = go 0 l
  where
  go :: Int -> List a -> Int
  go acc Nil = acc
  go acc (_ : xs) = go (acc + 1) xs

head :: ∀ a. List a -> Maybe a
head Nil = Nothing
head (x : _) = Just x

tail :: ∀ a. List a -> Maybe (List a)
tail Nil = Nothing
tail (_ : xs) = Just xs

last :: ∀ a. List a -> Maybe a
last Nil = Nothing
last (x : Nil) = Just x
last (_ : xs) = last xs

init :: ∀ a. List a -> Maybe (List a)
init Nil = Nothing
init (_ : Nil) = Just Nil
init (x : _ : Nil) = Just (x : Nil)
init l = Just $ go l
  where
  go Nil = Nil
  go (_ : Nil) = Nil
  go (x : xs) = x : go xs

uncons :: ∀ a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil = Nothing
uncons (x : xs) = Just { head: x, tail: xs }

index :: ∀ a. List a -> Int -> Maybe a
index Nil _ = Nothing
index _ i | i < 0 = Nothing
index (x : _) 0 = Just x
index (_ : xs) i = index xs (i - 1)

infixl 8 index as !!

findIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findIndex pred = go 0
  where
  go _ Nil = Nothing
  go i (x : xs)
    | pred x = Just i
    | otherwise = go (i + 1) xs

findLastIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex pred = go Nothing 0
  where
  go res _ Nil = res
  go res i (x : xs)
    | pred x = go (Just i) (i + 1) xs
    | otherwise = go res (i + 1) xs

reverse :: List ~> List
reverse Nil = Nil
reverse ol = go Nil ol
  where
  go rl Nil = rl
  go rl (x : xs) = go (x : rl) xs

concat :: ∀ a. List (List a) -> List a
concat Nil = Nil
concat (Nil : xss) = concat xss
concat ((x : xs) : xss) = x : concat (xs : xss)

filter' :: ∀ a. (a -> Boolean) -> List a -> List a
filter' _ Nil = Nil
filter' pred (x : xs)
  | pred x = x : filter' pred xs
  | otherwise = filter' pred xs

-- tail recursive, point-free version
filter :: ∀ a. (a -> Boolean) -> List a -> List a
filter pred = reverse <<< go Nil
  where
  go acc Nil = acc
  go acc (x : xs)
    | pred x = go (x : acc) xs
    | otherwise = go acc xs

catMaybes' :: ∀ a. List (Maybe a) -> List a
catMaybes' Nil = Nil
catMaybes' (Nothing : xs) = catMaybes' xs
catMaybes' (Just x : xs) = x : catMaybes' xs

catMaybes :: ∀ a. List (Maybe a) -> List a
catMaybes Nil = Nil
catMaybes (x : xs) = case x of
  Just y -> y : catMaybes xs
  Nothing -> catMaybes xs

range :: Int -> Int -> List Int
range x y
  | x < y = x : range (x + 1) y
  | x > y = x : range (x - 1) y
  | otherwise = singleton y

take' :: ∀ a. Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' i (x : xs) = x : take' (i - 1) xs

-- tail recursive
take :: ∀ a. Int -> List a -> List a
take n = reverse <<< go Nil (max 0 n)
  where
  go acc 0 _ = acc
  go acc _ Nil = acc
  go acc n' (x : xs) = go (x : acc) (n' - 1) xs

drop :: ∀ a. Int -> List a -> List a
drop _ Nil = Nil
drop 0 l = l
drop n (_ : xs) = drop ((max 1 n) - 1) xs

-- tail recursive
takeWhile' :: ∀ a. (a -> Boolean) -> List a -> List a
takeWhile' pred = reverse <<< go Nil
  where
  go acc Nil = acc
  go acc (x : xs)
    | pred x = go (x : acc) xs
    | otherwise = go acc Nil

takeWhile :: ∀ a. (a -> Boolean) -> List a -> List a
takeWhile _ Nil = Nil
takeWhile pred (x : xs) =
  if pred x then x : takeWhile pred xs else Nil

dropWhile :: ∀ a. (a -> Boolean) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile pred l@(x : xs) =
  if pred x then dropWhile pred xs else l

takeEnd :: ∀ a. Int -> List a -> List a
takeEnd n = go >>> snd
  where
  --  go :: List a -> (Tuple Int List a)
  go Nil = Tuple 0 Nil
  go (x : xs) =
    go xs
      # \(Tuple c nl) -> Tuple (c + 1) $ if c < n then x : nl else nl

dropEnd :: ∀ a. Int -> List a -> List a
dropEnd n = go >>> snd
  where
  go Nil = Tuple 0 Nil
  go (x : xs) = go xs # \(Tuple c nl) -> Tuple (c + 1) $ if c < n then nl else x : nl

zip :: ∀ a b. List a -> List b -> List (Tuple a b)
zip Nil _ = Nil
zip _ Nil = Nil
zip (x : xs) (y : ys) = Tuple x y : zip xs ys

-- WRONG
unzip' :: ∀ a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip' = go (Tuple Nil Nil)
  where
  go acc Nil = acc
  go (Tuple xs ys) ((Tuple x y) : ts) = go (Tuple (x : xs) (y : ys)) ts

unzip :: ∀ a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip Nil = Tuple Nil Nil
unzip (Tuple x y : ts) =
  unzip ts # \(Tuple xs ys) -> Tuple (x : xs) (y : ys)

-----------------------------------------------------
-- Exec
test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  flip const 1 2 # show # log
  log "-- singleton"
  log $ show $ singleton "xyz"
  log "-- null"
  log $ show $ null Nil
  log $ show $ null ("abc" : Nil)
  log "-- snoc"
  log $ show $ snoc (1 : 2 : Nil) 3
  log "-- length"
  log $ show $ length $ 1 : 2 : 3 : Nil
  log "-- head"
  log $ show $ head (Nil :: List Unit)
  log $ show $ head ("abc" : "123" : Nil)
  log "-- tail"
  log $ show $ tail (Nil :: List Unit)
  log $ show $ tail ("abc" : "123" : Nil)
  log "-- last"
  log $ show $ last (Nil :: List Unit)
  log $ show $ last ("a" : "b" : "c" : Nil)
  log "-- init"
  log $ show $ init (Nil :: List Unit)
  log $ show $ init (1 : Nil)
  log $ show $ init (1 : 2 : Nil)
  log $ show $ init (1 : 2 : 3 : Nil)
  log "-- uncons"
  log $ show $ uncons (1 : 2 : 3 : Nil)
  log "-- index"
  log $ show $ index (1 : Nil) 4
  log $ show $ index (1 : 2 : 3 : Nil) 1
  log $ show $ index (Nil :: List Unit) 0
  log $ show $ index (1 : 2 : 3 : Nil) (-99)
  log $ show $ (1 : 2 : 3 : Nil) !! 1
  log "-- findIndex"
  log $ show $ findIndex (_ >= 2) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (_ >= 99) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (10 /= _) (Nil :: List Int)
  log "-- findLastIndex"
  log $ show $ findLastIndex (_ == 10) (Nil :: List Int)
  log $ show $ findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil)
  log $ show $ findLastIndex (_ == 10) (11 : 12 : Nil)
  log "-- reverse"
  log $ show $ reverse (10 : 20 : 30 : Nil)
  log "-- concat"
  log $ show $ concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : Nil)
  log "-- filter"
  log $ show $ filter (4 > _) $ (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log "-- catMaybes"
  log $ show $ catMaybes (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil)
  log "-- range"
  log $ show $ range 1 10
  log $ show $ range 3 (-3)
  log "-- take"
  log $ show $ take 5 (12 : 13 : 14 : Nil)
  log $ show $ take 5 (-7 : 9 : 0 : 12 : -13 : 45 : 976 : -19 : Nil)
  log "-- drop"
  log $ show $ drop 2 (1 : 2 : 3 : 4 : 5 : 6 : 7 : Nil)
  log $ show $ drop 10 (Nil :: List Unit)
  log "-- takeWhile"
  log $ show $ takeWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
  log $ show $ takeWhile (_ == -17) (1 : 2 : 3 : Nil)
  log "-- dropWhile"
  log $ show $ dropWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
  log $ show $ dropWhile (_ == -17) (1 : 2 : 3 : Nil)
  log "-- takeEnd"
  log $ show $ takeEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $ takeEnd 10 (1 : Nil)
  log "-- dropEnd"
  log $ show $ dropEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $ dropEnd 10 (1 : Nil)
  log "-- zip"
  log $ show $ zip (1 : 2 : 3 : Nil) ("a" : "b" : "c" : "d" : "e" : Nil)
  log $ show $ zip ("a" : "b" : "c" : "d" : "e" : Nil) (1 : 2 : 3 : Nil)
  log $ show $ zip (Nil :: List Unit) (1 : 2 : Nil)
  log "-- unzip"
  log $ show $ unzip (Tuple 1 "a" : Tuple 2 "b" : Tuple 3 "c" : Nil)
  log $ show $ unzip (Tuple "a" 1 : Tuple "b" 2 : Tuple "c" 3 : Nil)
  log $ show $ unzip (Nil :: List (Tuple Unit Unit))

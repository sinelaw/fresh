{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
module Fresh.OrderedMap where

import qualified Data.Map as Map
import           Data.Map (Map)


data OrderedMap k a = OrderedMap { osList :: [(k, a)], osMap :: Map k a }
    deriving (Show, Functor)

instance (Eq k, Eq a) => Eq (OrderedMap k a) where
    o1 == o2 = osList o1 == osList o2
    o1 /= o2 = osList o1 /= osList o2

instance (Ord k, Ord a) => Ord (OrderedMap k a) where
    o1 `compare` o2 = osList o1 `compare` osList o2

instance Foldable (OrderedMap k) where
    foldr f x (OrderedMap xs _) = foldr (f . snd) x xs

instance Ord k => Traversable (OrderedMap k) where
    traverse f (OrderedMap xs _) = fromList <$> traverse (\(k,a) -> (k,) <$> f a) xs

null :: OrderedMap k a -> Bool
null (OrderedMap [] _) = True
null _ = False

empty :: OrderedMap k a
empty = OrderedMap [] Map.empty

singleton :: Ord k => k -> a -> OrderedMap k a
singleton k x = OrderedMap [(k, x)] (Map.singleton k x)

member :: Ord k => k -> OrderedMap k a -> Bool
member x (OrderedMap _ ss) = x `Map.member` ss

insert :: Ord k => k -> a -> OrderedMap k a -> OrderedMap k a
insert k x os@(OrderedMap kxs ss) =
    if k `Map.member` ss
    then os
    else OrderedMap ((k,x):kxs) (Map.insert k x ss)

delete :: Ord k => k -> OrderedMap k a -> OrderedMap k a
delete k os@(OrderedMap kxs ss) =
    if not (k `Map.member` ss)
    then os
    else OrderedMap (filter ((/= k) . fst) kxs) (Map.delete k ss)

fromList :: Ord k => [(k, a)] -> OrderedMap k a
fromList = foldr (uncurry insert) empty

toList :: OrderedMap k a -> [(k, a)]
toList (OrderedMap xs _) = xs

toMap :: OrderedMap k a -> Map k a
toMap (OrderedMap _ ss) = ss

difference :: Ord k => OrderedMap k a -> OrderedMap k a -> OrderedMap k a
difference (OrderedMap xs sxs) (OrderedMap _ sys) =
    OrderedMap (filter (\(k,_) -> k `Map.member` ds) xs) ds
    where ds = sxs `Map.difference` sys

intersection :: Ord k => OrderedMap k a -> OrderedMap k a -> OrderedMap k a
intersection (OrderedMap xs sxs) (OrderedMap _ sys) =
    OrderedMap (filter (\(k,_) -> k `Map.member` is) xs) is
    where is = sxs `Map.intersection` sys

-- left-biased: if two keys are the same, picks the value from the first map
concatUnion :: Ord k => OrderedMap k a -> OrderedMap k a -> OrderedMap k a
concatUnion o1 o2 = fromList (toList o1 ++ toList o2) -- TODO faster

-- left-biased: if two mapped keys are the same, picks the value at the first
mapKeys :: Ord k2 => (k1 -> k2) -> OrderedMap k1 a -> OrderedMap k2 a
mapKeys f = fromList . map (\(k,x) -> (f k, x)) . toList

mapWithKey :: (k -> a -> b) -> OrderedMap k a -> OrderedMap k b
mapWithKey f (OrderedMap kxs ss) = OrderedMap (map (\(k,x) -> (k, f k x)) kxs) (Map.mapWithKey f ss)

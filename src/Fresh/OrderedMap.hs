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
insert k x os@(OrderedMap xs ss) = if k `Map.member` ss
                                   then os
                                   else OrderedMap ((k,x):xs) (Map.insert k x ss)

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

-- concatUnion :: Ord k => OrderedMap k a -> OrderedMap k a -> OrderedMap k a
-- concatUnion = foldr2 insert

-- concatUnions :: Ord k => [OrderedMap k a] -> OrderedMap k a
-- concatUnions [] = empty
-- concatUnions (o:os) = foldr concatUnion o os

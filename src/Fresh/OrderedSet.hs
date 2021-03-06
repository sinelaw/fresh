module Fresh.OrderedSet where

import qualified Data.Set as Set
import           Data.Set (Set)


data OrderedSet a = OrderedSet { osList :: [a], osSet :: Set a }
    deriving (Show)

instance Eq a => Eq (OrderedSet a) where
    o1 == o2 = osList o1 == osList o2
    o1 /= o2 = osList o1 /= osList o2

instance Ord a => Ord (OrderedSet a) where
    o1 `compare` o2 = osList o1 `compare` osList o2

instance Foldable OrderedSet where
    foldr f x (OrderedSet xs _) = foldr f x xs

ordTraverse :: (Ord b, Applicative f) => (a -> f b) -> OrderedSet a -> f (OrderedSet b)
ordTraverse f (OrderedSet xs _) = fromList <$> traverse f xs

null :: Ord a => OrderedSet a -> Bool
null (OrderedSet [] _) = True
null _ = False

empty :: OrderedSet a
empty = OrderedSet [] Set.empty

singleton :: a -> OrderedSet a
singleton x = OrderedSet [x] (Set.singleton x)

member :: Ord a => a -> OrderedSet a -> Bool
member x (OrderedSet _ ss) = x `Set.member` ss

insert :: Ord a => a -> OrderedSet a -> OrderedSet a
insert x os@(OrderedSet xs ss) = if x `Set.member` ss
                                 then os
                                 else OrderedSet (x:xs) (Set.insert x ss)

fromList :: Ord a => [a] -> OrderedSet a
fromList = foldr insert empty

toList :: OrderedSet a -> [a]
toList (OrderedSet xs _) = xs

toSet :: OrderedSet a -> Set a
toSet (OrderedSet _ ss) = ss

difference :: Ord a => OrderedSet a -> OrderedSet a -> OrderedSet a
difference (OrderedSet xs sxs) (OrderedSet _ sys) =
    OrderedSet (filter (`Set.member` ds) xs) ds
    where ds = sxs `Set.difference` sys

intersection :: Ord a => OrderedSet a -> OrderedSet a -> OrderedSet a
intersection (OrderedSet xs sxs) (OrderedSet _ sys) =
    OrderedSet (filter (`Set.member` is) xs) is
    where is = sxs `Set.intersection` sys

concatUnion :: Ord a => OrderedSet a -> OrderedSet a -> OrderedSet a
concatUnion = foldr insert

concatUnions :: Ord a => [OrderedSet a] -> OrderedSet a
concatUnions [] = empty
concatUnions (o:os) = foldr concatUnion o os

{-# LANGUAGE MultiParamTypeClasses #-}
-- |

module Fresh.Subst
       ( SubstTV(..)
       , Subst
       , empty
       , delete
       , lookup
       ) where

import Prelude hiding (lookup)
import Data.Map (Map)
import qualified Data.Map as Map

newtype Subst k v = Subst (Map k v)
                  deriving (Ord, Eq, Show)

-- substLift :: (Map TyVar t -> Map TyVar u) -> (Subst t) -> (Subst u)
-- substLift fm (Subst m) = Subst (fm m)

empty :: Subst k v
empty = Subst Map.empty

delete :: Ord k => Subst k v -> [k] -> Subst k v
delete (Subst m) ks =
    Subst $ foldr (\tv m' -> Map.delete tv m') m ks

lookup :: Ord k => Subst k v -> k -> v -> v
lookup (Subst m) k vDef =
    case Map.lookup k m of
    Nothing -> vDef
    Just v -> v

class SubstTV k v t where
    subst :: Subst k v -> t -> t


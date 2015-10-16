-- |

module Fresh.Type where

import qualified Fresh.Kind as Kind
import           Fresh.Kind (Kind)

import Data.List ((\\))

newtype Id = Id String

data TyVar = TyVar String Kind
           deriving (Eq, Ord)

data TyCon = TyCon Id Kind

data Type t = TVar TyVar
            | TQuant [TyVar] t
            | TApp TyCon [t]

class FreeTV t where
    ftv :: t -> [TyVar]

instance FreeTV t => FreeTV (Type t) where
    ftv (TVar v) = [v]
    ftv (TApp con ts) = concatMap ftv ts
    ftv (TQuant qs t) = ftv t \\ qs


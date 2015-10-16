-- |

module Fresh.Type where

--import qualified Fresh.Kind as Kind
import           Fresh.Kind (Kind)

import qualified Data.Map as Map
import           Data.Map (Map)

import Data.List ((\\))

newtype Id = Id String
           deriving (Eq, Ord, Show)

data TyVar = TyVar String Kind
           deriving (Eq, Ord, Show)

data TyCon = TyCon Id Kind
           deriving (Eq, Ord, Show)

data Type t = TVar TyVar
            | TApp (TyApp t)
            | TQuant (TyQuant t)
            deriving (Eq, Ord, Show)

data TyQuant t = TyQuant [TyVar] t
               deriving (Eq, Ord, Show)

data TyApp t = TyApp TyCon [t]
             deriving (Eq, Ord, Show)

----------------------------------------------------------------------
-- free type vars

class FreeTV t where
    ftv :: t -> [TyVar]

instance FreeTV TyVar where
    ftv v = [v]

instance FreeTV t => FreeTV (TyQuant t) where
    ftv (TyQuant qs t) = ftv t \\ qs

instance FreeTV t => FreeTV (TyApp t) where
    ftv (TyApp _con ts) = concatMap ftv ts

instance FreeTV t => FreeTV (Type t) where
    ftv (TVar v) = ftv v
    ftv (TApp t) = ftv t
    ftv (TQuant q) = ftv q

----------------------------------------------------------------------
newtype Subst t = Subst (Map TyVar t)
                deriving (Ord, Eq, Show)

-- substLift :: (Map TyVar t -> Map TyVar u) -> (Subst t) -> (Subst u)
-- substLift fm (Subst m) = Subst (fm m)

substEmpty :: Subst t
substEmpty = Subst Map.empty

substDelete :: Subst t -> [TyVar] -> Subst t
substDelete (Subst m) tvs =
    Subst $ foldr (\tv m' -> Map.delete tv m') m tvs

class SubstTV t where
    subst :: Subst t -> t -> t

instance SubstTV TyVar where
    subst (Subst m) v =
        case Map.lookup v m of
        Nothing -> v
        Just v' -> v'

-- instance Subst t => Subst (TyQuant t) where
--     subst (Subst m) (TyQuant vs t) =
----------------------------------------------------------------------

newtype TypeEnv e t = TypeEnv (Map e t)

extendEnv :: Ord e => TypeEnv e t -> e -> t -> TypeEnv e t
extendEnv (TypeEnv m) e t = TypeEnv $ Map.insert e t m


data Expr t e = Var Id
              | App e e
              | Lam Id e
              | AnnLam Id t e
              | Let Id e e
              deriving (Show, Eq, Ord)




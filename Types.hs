module Main where

import Data.IORef

type Varname = String

data Expr
  = Var VarName
  | App Expr Expr
  | Lam VarName Expr
  | Let VarName Expr Expr

type QName = String

data TV
  = Unbound String
  | Link Type
  deriving (Eq)
    
data Type
  = TVar (IORef TV)
  | QVar QName
  | TArrow Type Type
  deriving (Eq)

unify :: Type -> Type -> Infer ()
unify t1 t2
  | t1 == t2 = IO ()
  | otherwise = unfiyNeq t1 t2

unifyNeq (TVar rtv1) t2 = unifyTVar rtv1 t2
unifyNeq t1 (TVar rtv2) = unifyTVar rtv2 t1
unifyNeq (TArrow a1 b1) (TArrow a2 b2) = do
  unify a1 a2
  unify b1 b2

unifyTVar ioTV t2 = do
  tv <- readIORef ioTV
  case tv of
    Unbound _ -> do
      occurs tv t2
      writeIORef ioTV t2
    Link t1 -> unify t1 t2

    
  

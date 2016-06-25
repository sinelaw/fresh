{-# LANGUAGE OverloadedStrings #-}
module Fresh.BuiltIn where

import Fresh.Types
import Fresh.Module (Module(..))
import Fresh.Expr (Expr(..), EVarName(..), ETypeAsc(..))
import Fresh.Kind (Kind(..))

import qualified Data.Map as Map


tcon :: String -> Type
tcon x = Fix $ TyCon $ TCon (Id x) Star

_String :: Type
_String = tcon "String"

_Bool :: Type
_Bool = tcon "Bool"

_Int :: Type
_Int =  tcon "Int"

_Func :: Type
_Func = Fix tyFunc

(~=>) :: [Pred t] -> t -> QualType t
(~=>) = QualType

(^$) :: Type -> Type -> Type
f ^$ x = Fix $ TyAp f x

infixr 5 ^->
(^->) :: Type -> Type -> Type
targ ^-> tres = Fix $ TyAp (Fix $ TyAp _Func targ) tres

----------------------------------------------------------------------

gv0 :: GenVar ()
gv0 = GenVar { genVarId = 0
             , genVarKind = Star
             , genVarAnnot = () }

genericBinOp :: QualType Type
genericBinOp = QualType [] (a ^-> a ^-> a)
    where a = Fix $ TyGenVar gv0

-- Classes

numClass :: Class Type (Expr ())
numClass
    = Class
      { clsId = ClassId "Num"
      , clsSupers = []
      , clsParam = gv0
      , clsMembers = Map.fromList
          [ (MemberName "+", genericBinOp)
          , (MemberName "-", genericBinOp)
          , (MemberName "*", genericBinOp)
          , (MemberName "/", genericBinOp)
          ]
      , clsInstances = [intNumInstance]
      }


----------------------------------------------------------------------

binaryOp :: Type -> QualType Type
binaryOp t = QualType [] $ t ^-> t ^-> t

ebinOp :: Type -> String -> (EVarName, Expr ())
ebinOp t name = (vname, EBuiltIn () vname (ETypeAsc $ binaryOp t))
    where vname = (EVarName $ "#int" ++ name)

intBinOp :: String -> (EVarName, Expr ())
intBinOp = ebinOp _Int

intPlus, intMinus, intMul, intDiv :: (EVarName, Expr())
intPlus = intBinOp "+"
intMinus = intBinOp "-"
intMul = intBinOp "*"
intDiv = intBinOp "/"

intNumInstance :: Instance Type (Expr ())
intNumInstance = Instance (ClassId "Num") (QualType [] _Int) m
    where m = Map.fromList
              [ (MemberName "+", snd intPlus)
              , (MemberName "-", snd intMinus)
              , (MemberName "*", snd intMinus)
              , (MemberName "/", snd intMinus)
              ]

intBuiltIn :: Module Type (Expr ())
intBuiltIn
    = Module
    { moduleTypes = Map.fromList
        [ (Id "Int", _Int)
        ]
    , moduleExprs = Map.fromList
        [ intPlus
        , intMinus
        , intMul
        , intDiv
        ]
    , moduleClasses = Map.empty
    }
----------------------------------------------------------------------

boolBinOp :: String -> (EVarName, Expr ())
boolBinOp = ebinOp _Bool

boolBuiltIn :: Module Type (Expr ())
boolBuiltIn
    = Module
    { moduleTypes = Map.fromList
        [ (Id "Bool", _Bool)
        ]
    , moduleExprs = Map.fromList
        [ boolBinOp "&&"
        , boolBinOp "||"
        ]
    , moduleClasses = Map.empty
    }

----------------------------------------------------------------------

builtIns :: Module Type (Expr ())
builtIns = mconcat [boolBuiltIn, intBuiltIn]

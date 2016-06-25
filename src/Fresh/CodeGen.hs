{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
module Fresh.CodeGen where

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Fresh.Expr
import Fresh.Infer ()
import Fresh.Types



import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans.State (StateT(..), get, put)
-- import Control.Monad.State.Class (MonadState(..), modify)
-- import Control.Monad.Trans.Either (EitherT(..))
-- import Control.Monad.Error.Class (MonadError(..))


import qualified Data.Map as Map
import           Data.Map ( Map )
import Data.String (IsString(..))

import qualified Fresh.OrderedMap as OrderedMap
import           Fresh.OrderedMap ( OrderedMap )

data TagType
data TagExpr

data GName x = GName String
    deriving (Show, Eq, Ord)

instance IsString (GName x) where
    fromString = GName

instance Monoid (GName x) where
    mempty = GName mempty
    (GName a) `mappend` (GName b) = GName (a `mappend` b)

type EName = GName TagExpr
type TName = GName TagType

data GFuncType a t = GFuncType a (OrderedMap EName t) t
    deriving (Show, Eq, Functor, Foldable, Traversable)

data GLit
    = GLitDouble Double
    | GLitInt Int
    | GLitStr String
    | GLitBool Bool
    deriving (Show, Eq, Ord)

data GType a t
    = GTNamed a TName
    | GTFunction a (GFuncType a t)
    | GTStruct a (OrderedMap EName t)
    | GTUnion a (Map EName t)
    | GTArray a t
    | GTPointer a t
--    | GTGeneric a TName [t]
    deriving (Show, Eq, Functor, Foldable, Traversable)

data GQual = GQConst
    deriving (Show, Eq)

data GQualType t
    = GQualType [GQual] t
    deriving (Show, Eq, Functor, Foldable, Traversable)

data GTypeDecl a t = GTypeDecl a t TName
    deriving (Show, Eq, Functor, Foldable, Traversable)

data GDef a t
    = GDFuncDef a (GFuncType a t) EName (GStmt a t)
    | GDVarDef a t EName (GExpr a t)
    deriving (Show, Eq, Functor, Foldable, Traversable)

data GDecl a t
    = GDFuncDecl a (GFuncType a t) EName
    | GDVarDecl a t EName
    deriving (Show, Eq, Functor, Foldable, Traversable)

data GScope a t
    = GScope a [GDef a t] (GStmt a t)
    deriving (Show, Eq, Functor, Foldable, Traversable)

data GStmt a t
    = GSVarWrite a EName (GExpr a t)
    | GSExpr a (GExpr a t)
    | GSBlock a [GStmt a t]
    | GSScope a (GScope a t)
    | GSIf a (GExpr a t) (GStmt a t) (GStmt a t)
    | GSReturn a (GExpr a t)
    deriving (Show, Eq, Functor, Foldable, Traversable)

data GLValue a t
    = GLVarRead a EName
    | GLPtrDeref a (GExpr a t)
    | GLArrayDeref a (GExpr a t) (GExpr a t)
    | GLField a (GExpr a t) EName
    deriving (Show, Eq, Functor, Foldable, Traversable)

data GExpr a t
    = GELValue a (GLValue a t)
    | GEAddrOf a (GLValue a t)
    | GECall a (GExpr a t) [GExpr a t]
    | GELit a GLit
    | GECompound a (GScope a t)
    | GERowLit a TName (OrderedMap EName (GExpr a t)) -- union/struct literal
    deriving (Show, Eq, Functor, Foldable, Traversable)


generateLit :: a -> Lit -> GExpr a t
generateLit a (LitNum d) = GELit a (GLitDouble d)
generateLit a (LitString s) = GELit a (GLitStr s)
generateLit a (LitBool b) = GELit a (GLitBool b)

getVars :: Expr a -> OrderedMap EVarName a
getVars ELit{} = OrderedMap.empty
getVars (EVar a name) = OrderedMap.singleton name a
getVars (ELam _ arg body) = OrderedMap.delete arg $ getVars body
getVars (EALam _ arg _ body) = OrderedMap.delete arg $ getVars body
getVars (EApp _ f x) = getVars f `OrderedMap.concatUnion` getVars x
getVars (ELet _ name val body) = OrderedMap.delete name (getVars val `OrderedMap.concatUnion` getVars body)
getVars (EAsc _ _ body) = getVars body
getVars (EGetField _ row _) = getVars row
getVars EBuiltIn{} = OrderedMap.empty

--newtype GTypeQ a = GTypeQ (GType a (GQualType (GTypeQ a)))

newtype GTypeQ a t = GTypeQ (GQualType (GType a t))
    deriving (Show, Functor)

type GTyQ a = Fix (GTypeQ a)
type GTy = GTyQ ()
type Decl = GDecl () GTy
type Def = GDef () GTy
type TyDecl = GTypeDecl () GTy

gType :: GType a (Fix (GTypeQ a)) -> Fix (GTypeQ a)
gType = Fix . GTypeQ . GQualType []

unGType :: GTyQ a -> GType a (GTyQ a)
unGType (Fix (GTypeQ (GQualType q t))) = t

fromTCon :: TCon -> TName
fromTCon (TCon (Id s) k) = GName s

fromComposite :: Composite Type -> [(EName, GTy)]
fromComposite c =
    map (\(CompositeLabelName n, v) -> (GName n, fromType v))
    $ Map.toList
    $ fcLabels
    $ flattenComposite c

fromFlatTy :: (Type, [Type]) -> GTy

fromFlatTy (Fix (TyAp{}), _   )  = error "Stray TyAp after flattenning!"
fromFlatTy (Fix (TyGen{}), _  )  = error "Can't convert TyGen!"
fromFlatTy (Fix (TyGenVar{}), _) = error "Can't convert TyGenVar!"
fromFlatTy (Fix (TyComp{}), _)   = error "Unadorned TyComp after flattenning!"

fromFlatTy t@(Fix tf, (arg:rest))
    | tf == tyFunc = gType $ GTFunction () (uncurry funcType $ go arg rest)
    where
        funcType args res = GFuncType () (OrderedMap.fromList
                                          $ zip (map (GName . ('x':) . show) [0..])
                                          $ map fromType args)
                            (fromType res)
        go x [y] = ([x], y)
        go x (Fix tf':y:ys)
            | tf' == tyFunc =
                  let (args', res) = go y ys
                  in (x:args', res)
        go _ _ = error $ "Can't uncurry: " ++ (show $ pretty t)

fromFlatTy (Fix tf, [Fix (TyComp c)])
    | tf == tyRec  = gType $ GTStruct () (OrderedMap.fromList $ fromComposite c)
    | tf == tySum  = gType $ GTUnion  () (       Map.fromList $ fromComposite c)
    | otherwise    = error ("Unknown composite type constructor: " ++ show tf)

fromFlatTy (Fix (TyCon tc), [])    = gType $ GTNamed   () (fromTCon tc)
--fromFlatTy (Fix (TyCon tc), targs) = gType $ GTGeneric () (fromTCon tc) (map fromType targs)

fromType :: Type -> GTy
fromType = fromFlatTy . flatTyHead . flattenTyAp (Just . unFix)

data GenerateState
    = GenerateState
      { gsDecls :: [Decl]
      , gsTypeDecls :: [TyDecl]
      , gsDefs :: [Def]
      , gsCounter :: Int
      }
      deriving (Show)

generateStateEmpty :: GenerateState
generateStateEmpty
    = GenerateState
    { gsDecls = []
    , gsTypeDecls = []
    , gsDefs = []
    , gsCounter = 0
    }

type GenerateT m a = StateT GenerateState m a

type Generate a = GenerateT Identity a

--runGenerate :: Generate (GStmt () GTy) -> (GStmt () GTy)
runGenerate :: StateT GenerateState Identity a -> (a, GenerateState)
runGenerate x = runIdentity $ runStateT x generateStateEmpty

genName :: GName x -> Generate (GName x)
genName (GName name) = do
    s <- get
    let s' = s { gsCounter = gsCounter s + 1 }
    put s'
    return $ GName (name ++ (show $ gsCounter s))

putTypeDecl :: TyDecl -> Generate ()
putTypeDecl d = do
    s <- get
    put $ s { gsTypeDecls = d : gsTypeDecls s
            , gsCounter = gsCounter s
            }

putDecl :: Decl -> Generate ()
putDecl d = do
    s <- get
    put $ s { gsDecls = d : gsDecls s
            , gsCounter = gsCounter s
            }

putDef :: Def -> Generate ()
putDef d = do
    s <- get
    put $ s { gsDefs = d : gsDefs s
            , gsCounter = gsCounter s
            }

generateType :: GTy -> Generate GTy
generateType (Fix (GTypeQ (GQualType qs (GTFunction _ (GFuncType _ args res))))) = do
    gargs <- OrderedMap.fromList <$> mapM (\(n, x) -> (n,) <$> generateType x) (OrderedMap.toList args)
    gres <- generateType res
    let gty = Fix (GTypeQ (GQualType [] (GTFunction () (GFuncType () gargs gres))))
    tName <- genName "Func"
    putTypeDecl (GTypeDecl () gty tName)
    return (Fix (GTypeQ (GQualType qs (GTNamed () tName))))
generateType t = return t

generateFunDef :: EName -> GFuncType () GTy -> t -> GStmt () GTy -> Generate ()
generateFunDef name ft arg body = do
    putDef (GDFuncDef () ft name body)
    return ()

-- mkClosure :: OrderedMap EName t
--     -> String
--     -> Generate (GExpr a t)
mkClosure closureArgs funcName = do
    let closureType = gType $ GTStruct () closureArgs
        closure = OrderedMap.mapWithKey (\k _ -> GELValue () (GLVarRead () k)) closureArgs
    closureTypeName <- genName $ GName $ funcName  `mappend` "_closure"
    putTypeDecl (GTypeDecl () closureType closureTypeName)
    wrapperTypeName <- genName $ GName $ funcName `mappend` "_wrapper"
    let wrapperType :: GTy
        wrapperType =
            gType $ GTStruct ()
            $ OrderedMap.fromList
              [("closure", gType $ GTNamed () closureTypeName)
              ,("func", gType $ GTNamed () $ GName funcName)]
    putTypeDecl (GTypeDecl () wrapperType wrapperTypeName)
    return $ (closureTypeName, GERowLit () wrapperTypeName
        $ OrderedMap.fromList
        $ [ ("closure", GERowLit () closureTypeName closure)
          , ("func"   , GELValue () (GLVarRead () $ GName funcName))
          ])
generateFun :: String -> Type -> EVarName
            -> Expr Type
            -> Generate (GExpr () GTy)
generateFun name t (EVarName arg) body = do
    fName <- genName (GName name)
    let gt = fromType t
    case unGType gt of
        GTFunction _ (GFuncType a mgtargs gtres) -> do
            let closureArgs =
                    filter (\(cn, ct) -> cn /= GName arg)
                    $ map (\(EVarName cs, ct) -> (GName cs, fromType ct))
                    $ OrderedMap.toList
                    $ getVars body
                closureArgsMap =  OrderedMap.fromList closureArgs
                -- dropping name from type because it is x0, x1, ...
                -- expecting all functions to be curried (single arg)
                getArgType [(_gtargName, gtargType)] = gtargType
                getArgType [] = error "No arg?"
                getArgType xs = error $ "More than one arg? " ++ show xs

            (closureTypeName, closureLit) <- mkClosure closureArgsMap name
            gbody <- GSExpr () <$> generate body -- TODO
            let allArgs = OrderedMap.concatUnion
                          (OrderedMap.fromList [("closure", gType $ GTNamed () closureTypeName)])
                          (OrderedMap.fromList [(GName arg, getArgType $ OrderedMap.toList mgtargs)])
                closureVarDefs =
                    map (\(cn, ct) -> GDVarDef () ct cn (GELValue () (GLField () (GELValue () (GLVarRead () "closure")) cn)))
                    $ closureArgs
                scopedBody = GSReturn () $ GECompound () (GScope () closureVarDefs gbody)
            generateFunDef fName (GFuncType a allArgs gtres) arg scopedBody
            return closureLit
--            return (GELValue () (GLVarRead () fName), closureArgsMap)
        _ -> error $ "Not a function type: " ++ (show $ pretty t) ++ "\n\t" ++ show t

generateDef :: EVarName -> Expr Type -> Generate Def
generateDef (EVarName s) val = do
    -- TODO check if func
    gt <- generateType $ fromType $ getAnnotation val
    gs <- generate val
    return $ GDVarDef () gt (GName s) gs

dropQual :: Expr (QualType Type) -> Expr Type
dropQual expr = fmap go expr
    where
        go (QualType [] t) = t
        go (QualType ps t) = error "Type must be fully determined"

generate :: Expr Type -> Generate (GExpr () GTy)
generate (ELit _ l)                  = return $ generateLit () l
generate (EVar _ (EVarName s))       = return $ GELValue () (GLVarRead () (GName s))
generate (EBuiltIn a (EVarName s) _) = return $ GELValue () (GLVarRead () (GName s))
generate (ELam t arg body)           = generateFun "lambda" t arg body
generate (EALam t arg _tasc body)    = generateFun "lambda" t arg body
generate (EApp _ fun arg) = do
    gfun <- generate fun
    garg <- generate arg
    return $ GECall () (gfun) [garg]
generate (ELet t name val body) = do
    gdef <- generateDef name val
    gbody <- generate body
    return $ GECompound () (GScope () [gdef] (GSExpr () gbody))
generate (EAsc t asc body) = generate body
generate (EGetField t row (CompositeLabelName field)) = do
    grow <- generate row
    return $ GELValue () (GLField () grow $ GName field)

--     generate row >>= (\gr -> return $ "(" <> gr <> ")" <> "." <> pretty field)

instance Pretty (GName x) where
    pretty (GName s) = pretty s

prettyFuncType :: (Pretty t) => Doc -> GFuncType a t -> Doc
prettyFuncType name (GFuncType _ margs tres) =
    pretty tres <+> name
        <> tupled (map (\(pn, pt) -> pretty pt <+> pretty pn) (OrderedMap.toList margs))

instance (Pretty t) => Pretty (GFuncType a t) where
    pretty ft = prettyFuncType "(*)" ft

instance Pretty GLit where
    pretty (GLitDouble x) = pretty x
    pretty (GLitInt x) = pretty x
    pretty (GLitStr x) = pretty x
    pretty (GLitBool x) = pretty x

prettyFields :: (Pretty t, Pretty a) => [(t, a)] -> Doc
prettyFields margs =
    vsep
    $ map (\(pn, pt) -> pretty pt <+> pretty pn <> ";")
    $ margs

instance (Pretty t) => Pretty (GType a t) where
    pretty (GTNamed a gname) = pretty gname
    pretty (GTFunction a ft) = pretty ft
    pretty (GTStruct a margs) = vsep
        [ "struct {"
        , indent 4 $ prettyFields $ OrderedMap.toList margs
        , "}"
        ]
    pretty (GTUnion a margs) = vsep
        [ "union {"
        , indent 4 $ prettyFields $ Map.toList margs
        , "}"
        ]
    pretty (GTArray a t) = "[" <> pretty t <> "]"
    pretty (GTPointer a t) = pretty t <+> "*"
    -- pretty (GTGeneric a gname [t])

instance Pretty GQual where
    pretty GQConst = text "const"

instance Pretty t => Pretty (GQualType t) where
    pretty (GQualType qs t) = hsep (map pretty qs) <+> pretty t

instance Pretty t => Pretty (GTypeDecl a t) where
    pretty (GTypeDecl _ t name) =
        "typedef" <+> pretty t <+> pretty name

instance Pretty t => Pretty (GDef a t) where
    pretty (GDFuncDef _ ft name body) =
        vsep
        [ prettyFuncType (pretty name) ft
        , "{"
        , indent 4 $ pretty body
        , "}"
        ]
    pretty (GDVarDef _ t n val) = pretty t <+> pretty n <+> "=" <+> pretty val

instance Pretty t => Pretty (GDecl a t) where
    pretty (GDFuncDecl _ ft name) = prettyFuncType (pretty name) ft
    pretty (GDVarDecl _ t name) = pretty t <+> pretty name

instance Pretty t => Pretty (GScope a t) where
    pretty (GScope a defs stms) =
        vsep
        [ "{" -- TODO vars
        , indent 4 $ vsep $ map ((<> ";") . pretty) defs
        , indent 4 $ pretty stms
        , "}"
        ]

instance Pretty t => Pretty (GStmt a t) where
    pretty (GSVarWrite a gname e) =
        pretty gname <+> "=" <+> pretty e
    pretty (GSExpr a e) = pretty e
    pretty (GSScope a s) = pretty s
    pretty (GSIf a pred then' else') =
        vsep
        [ "if" <+> parens (pretty pred)
        , "{"
        , pretty then'
        , "} else {"
        , pretty else'
        , "}"
        ]
    pretty (GSReturn a r) = hang 4 $ "return" <+> pretty r

instance Pretty t => Pretty (GLValue a t) where
    pretty (GLVarRead a gname) = pretty gname
    pretty (GLPtrDeref a e) = parens ("*" <> pretty e)
    pretty (GLArrayDeref a arr idx) =
        pretty arr <> "[" <> pretty idx <> "]"
    pretty (GLField a e gname) = pretty e <> "." <> pretty gname

instance Pretty t => Pretty (GExpr a t) where
    pretty (GELValue a v) = pretty v
    pretty (GEAddrOf a l) = pretty l
    pretty (GECall a f args) =
        pretty f <> tupled (map pretty args)
    pretty (GELit a l) = pretty l
    pretty (GECompound a scope) = parens $ pretty scope
    pretty (GERowLit a name fields) =
        parens (pretty name)
        <> (vsep
            [ "{"
            , indent 4 $ vsep
              $ map (\(pn, pv) -> "." <> pretty pn <+> "=" <+> pretty pv <> ",")
              $ OrderedMap.toList fields
            , "}"])


instance Pretty t => Pretty (GTypeQ a t) where
    pretty (GTypeQ qt) = pretty qt

instance Pretty GenerateState where
    pretty gs = vsep
        [ "// Type declarations:"
        , vsep $ map ((<> ";") . pretty) (gsTypeDecls gs)
        , "// Declarations:"
        , vsep $ map pretty (gsDecls gs)
        , "// Definitions:"
        , vsep $ map pretty (gsDefs gs)
        ]


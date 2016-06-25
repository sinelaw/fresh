{-# LANGUAGE OverloadedStrings #-}
module Fresh.CodeGen where

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Fresh.Expr
import Fresh.InferMonad (callFrame)
import Fresh.Infer (matchFun', runInfer)
import Fresh.Types (unresolveQual, Type, QualType(..), TypeError(..)
                   ,CompositeLabelName(..), TypeError, Fix(..), TypeAST(..))
import Fresh.InferMonad (resolve, purify)

import Control.Monad.Identity (Identity, runIdentity)


import Control.Monad.Trans.State (StateT(..), get, put)
-- import Control.Monad.State.Class (MonadState(..), modify)
-- import Control.Monad.Trans.Either (EitherT(..))
-- import Control.Monad.Error.Class (MonadError(..))

import qualified Data.Map as Map



generateLit :: Lit -> Doc
generateLit (LitNum d) = pretty $ show d
generateLit (LitString s) = pretty $ show s
generateLit (LitBool b) = pretty . text $ if b then "true" else "false"

funcArgRes :: QualType Type
           -> Either TypeError (QualType Type, QualType Type)
funcArgRes (QualType _ (Fix (TyGen gvs q@(QualType ps _)))) = runInfer $ callFrame "funcArgRes" $ do
    let (QualType _ t') = unresolveQual q
    (targ, tres) <- matchFun' t'
    Just targ' <- resolve targ
    Just tres' <- resolve tres
    return (QualType ps targ', QualType ps tres')
funcArgRes q@(QualType ps _) = runInfer $ callFrame "funcArgRes" $ do
    let (QualType _ t') = unresolveQual q
    (targ, tres) <- matchFun' t'
    Just targ' <- resolve targ
    Just tres' <- resolve tres
    return (QualType ps targ', QualType ps tres')

getVars :: Expr a -> Map.Map EVarName a
getVars ELit{} = Map.empty
getVars (EVar a name) = Map.singleton name a
getVars (ELam _ arg body) = Map.delete arg $ getVars body
getVars (EALam _ arg _ body) = Map.delete arg $ getVars body
getVars (EApp _ f x) = getVars f `Map.union` getVars x
getVars (ELet _ name val body) = Map.delete name (getVars val `Map.union` getVars body)
getVars (EAsc _ _ body) = getVars body
getVars (EGetField _ row _) = getVars row
getVars EBuiltIn{} = Map.empty

data GenerateState
    = GenerateState
      { gsDefs :: Doc
      , gsCounter :: Int
      }

generateStateEmpty :: GenerateState
generateStateEmpty = GenerateState { gsDefs = empty, gsCounter = 0 }

type GenerateT m a = StateT GenerateState m a

type Generate a = GenerateT Identity a

formatGenerateState :: Doc -> GenerateState -> Doc
formatGenerateState d (GenerateState defs _) = defs <> linebreak <> d

runGenerate :: Generate Doc -> Doc
runGenerate x = uncurry formatGenerateState . runIdentity $ runStateT x generateStateEmpty

genName :: Doc -> Generate Doc
genName name = do
    s <- get
    let s' = s { gsCounter = gsCounter s + 1 }
    put s'
    return $ name <> pretty (gsCounter s)

genToContext :: Generate Doc -> Generate Doc
genToContext g = do
    d <- g
    s <- get
    put $ s { gsDefs = gsDefs s <> linebreak <> d
            , gsCounter = gsCounter s
            }
    return empty

generateType :: QualType Type -> Generate Doc
generateType q = case funcArgRes q of
    Left{} -> return $ pretty q
    Right (targ, tres) -> do
        tName <- genName "Func"
        gtres <- generateType tres
        gtarg <- generateType targ
        genToContext $ return $ "typedef" <+> gtres <+> tName <> parens gtarg <> ";"
        return tName

generateFunDef :: Doc -> QualType Type -> EVarName -> Expr (QualType Type) -> Generate Doc
generateFunDef name t arg body = genToContext $ do
    gtres <- generateType tres
    gbody <- generate body
    argDef <- (<+> pretty arg) <$> generateType targ
    closureArgsDef <- mapM (\(cName, cType) -> ( <+> pretty cName) <$> generateType cType) $ Map.toList vars
    let allArgsDef = (argDef : closureArgsDef)
    return $ vsep
        [ gtres <+> name <> tupled allArgsDef
        , "{"
        , indent 4 $ "return" <+> (hang 4 gbody) <> ";"
        , "}"
        ]
    where
        (targ, tres) =
            case funcArgRes t of
                Right (a,b) -> (a,b)
                Left x -> error ("In: " ++ show name ++ ": " ++ (show $ pretty x))
        vars = Map.delete arg $ getVars body

generateFun :: Doc -> QualType Type -> EVarName
            -> Expr (QualType Type)
            -> Generate Doc
generateFun name t arg body = do
    fName <- genName name
    generateFunDef fName t arg body
    return $ pretty fName

generateDef :: EVarName -> Expr (QualType Type) -> Generate Doc
generateDef (EVarName s) val = do
    gt <- generateType $ getAnnotation val
    gs <- generate val
    return $ gt <+> pretty s <+> "=" <+> gs <> ";"

generate :: Expr (QualType Type) -> Generate Doc
generate (ELit _ l) = return $ generateLit l
generate (EVar _ (EVarName s)) = return $ pretty s
generate (ELam t arg body) = generateFun "lambda" t arg body
generate (EALam t arg _tasc body) = generateFun "lambda" t arg body
generate (EApp _ fun arg) = do
    gfun <- generate fun
    garg <- generate arg
    return $ gfun <> "(" <> garg <> ")"
generate (ELet t name val body) = do
    gdef <- generateDef name val
    gbody <- generate body
    return $
        vsep [ "({"
             , indent 4 $ vsep [ gdef, gbody ]
             , "})"
             ]
generate (EAsc t asc body) = generate body
generate (EGetField t row (CompositeLabelName field)) =
    generate row >>= (\gr -> return $ "(" <> gr <> ")" <> "." <> pretty field)
generate (EBuiltIn a (EVarName s) _) = return $ pretty s

